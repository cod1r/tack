open Freetype
open Sdl
open Opengl
open Editor

module Render = struct
  external get_buffer_size : Opengl.buffer -> int
    = "get_buffer_size" "get_buffer_size"

  external init_buffer_with_capacity : int -> Opengl.buffer
    = "init_buffer_with_capacity" "init_buffer_with_capacity"

  external init_buffer : floats_per_point:int -> Opengl.buffer
    = "init_buffer" "init_buffer"

  external write_cursor_to_buffer :
    Opengl.buffer -> int * int -> int -> int -> unit
    = "write_cursor_to_buffer" "write_cursor_to_buffer"

  external write_to_buffer :
    Opengl.buffer ->
    FreeType.glyph_info ->
    window_dims:int * int ->
    x_offset:int ->
    font_height:int ->
    stride:int ->
    unit = "write_to_buffer" "write_to_buffer"
  [@@noalloc]

  external reset_buffer : Opengl.buffer -> unit = "reset_buffer" "reset_buffer"

  let _EACH_POINT_FLOAT_AMOUNT = 6
  let _EACH_POINT_FLOAT_AMOUNT_CURSOR = 2

  let vertex_shader =
    {|
  #version 120
  attribute vec2 point_vertex;
  attribute vec4 color;
  varying float alpha;
  void main() {
    gl_Position = vec4(point_vertex.x, point_vertex.y, 0.0, 1.0);
    alpha = color;
  }
  |}

  let fragment_shader =
    {|
  #version 120
  varying vec4 color;
  void main() {
    gl_FragColor = vec4(color.r, color.g, color.b, color.a);
  }
  |}

  let vertex_shader_cursor =
    {|
  #version 120
  attribute vec2 point;
  void main() {
    gl_Position = vec4(point.x, point.y, 0.0, 1.0);
  }
    |}

  let fragment_shader_cursor =
    {|
    #version 120
    void main() {
      gl_FragColor = vec4(0.0, 0.0, 0.0, 1.0);
    }
    |}

  let _ = gl_enable_blending ()
  let b = init_buffer ~floats_per_point:_EACH_POINT_FLOAT_AMOUNT

  (* 2 pixels wide * font_height * 2 floats per pixel *)
  let cursor_buffer = init_buffer_with_capacity (2 * FreeType.font_height * 2)

  let fragment =
    match gl_create_fragment_shader () with Ok f -> f | Error e -> failwith e

  let vertex =
    match gl_create_vertex_shader () with Ok v -> v | Error e -> failwith e

  let vertex_cursor =
    match gl_create_vertex_shader () with
    | Ok v -> v
    | Error e -> failwith (e ^ "_CURSOR")

  let fragment_cursor =
    match gl_create_fragment_shader () with
    | Ok f -> f
    | Error e -> failwith (e ^ "_CURSOR")

  let program =
    gl_shader_source fragment fragment_shader;
    gl_shader_source vertex vertex_shader;
    gl_compileshader fragment;
    if not (gl_get_shader_compile_status fragment) then
      failwith (gl_get_shader_info_log fragment);
    gl_compileshader vertex;
    if not (gl_get_shader_compile_status vertex) then
      failwith (gl_get_shader_info_log vertex);
    let p =
      match gl_createprogram () with Ok p -> p | Error e -> failwith e
    in
    gl_attach_shader p fragment;
    gl_attach_shader p vertex;
    gl_linkprogram p;
    p

  let program_cursor =
    gl_shader_source fragment_cursor fragment_shader_cursor;
    gl_shader_source vertex_cursor vertex_shader_cursor;
    gl_compileshader fragment_cursor;
    if not (gl_get_shader_compile_status fragment_cursor) then
      failwith (gl_get_shader_info_log fragment_cursor);
    gl_compileshader vertex_cursor;
    if not (gl_get_shader_compile_status vertex_cursor) then
      failwith (gl_get_shader_info_log vertex_cursor);
    let p =
      match gl_createprogram () with Ok p -> p | Error e -> failwith e
    in
    gl_attach_shader p fragment_cursor;
    gl_attach_shader p vertex_cursor;
    gl_linkprogram p;
    p

  let draw_editor (text_buffer : buffer) (cursor_buffer : buffer)
      (editor : Editor.editor) =
    let window_dims = Sdl.sdl_gl_getdrawablesize () in
    let window_width, window_height = window_dims in
    let fold_fn (acc : unit Editor.rope_traversal_info) c =
      if c = '\n' then (
        let div_ans =
          (acc.acc_horizontal_x_pos + window_width) / window_width
        in
        if acc.rope_pos = editor.Editor.cursor_pos then
          write_cursor_to_buffer cursor_buffer window_dims
            acc.acc_horizontal_x_pos FreeType.font_height;
        ({
           acc_horizontal_x_pos = div_ans * window_width;
           rope_pos = acc.rope_pos + 1;
           accumulation = ();
         }
          : unit Editor.rope_traversal_info))
      else
        let glyph_info_found =
          Array.find_opt (fun (c', _) -> c' = c) Editor.glyph_info_with_char
        in
        match glyph_info_found with
        | Some (_, gi) ->
            let x_advance = FreeType.get_x_advance gi in
            let plus_x_advance =
              (acc.acc_horizontal_x_pos + x_advance) / window_width
            and without_x_advance = acc.acc_horizontal_x_pos / window_width in
            let y_pos = without_x_advance * FreeType.font_height in
            if y_pos <= window_height && y_pos >= 0 then
              write_to_buffer text_buffer gi ~window_dims
                ~x_offset:acc.acc_horizontal_x_pos
                ~font_height:FreeType.font_height
                ~stride:_EACH_POINT_FLOAT_AMOUNT;
            let processed_acc_x_offset =
              if plus_x_advance > without_x_advance then
                plus_x_advance * window_width
              else acc.acc_horizontal_x_pos
            in
            if acc.rope_pos = editor.Editor.cursor_pos then
              write_cursor_to_buffer cursor_buffer window_dims
                processed_acc_x_offset FreeType.font_height;
            ({
               acc_horizontal_x_pos = processed_acc_x_offset + x_advance;
               rope_pos = acc.rope_pos + 1;
               accumulation = ();
             }
              : unit Editor.rope_traversal_info)
        | None ->
            Printf.printf "not found";
            print_char c;
            print_newline ();
            acc
    in
    let { Editor.acc_horizontal_x_pos; _ } =
      Editor.traverse_rope
        (editor.rope |> Option.get)
        fold_fn
        ({
           acc_horizontal_x_pos = editor.vertical_scroll_y_offset * window_width;
           rope_pos = 0;
           accumulation = ();
         }
          : unit Editor.rope_traversal_info)
    in
    if editor.cursor_pos = Rope.length (editor.rope |> Option.get) then
      write_cursor_to_buffer cursor_buffer window_dims acc_horizontal_x_pos
        FreeType.font_height;
    ()

  let gl_buffer_obj = gl_gen_one_buffer ()
  let gl_buffer_cursor = gl_gen_one_buffer ()

  let location_point_vertex =
    match gl_getattriblocation program "point_vertex" with
    | Ok l -> l
    | Error e -> failwith e

  let location_color =
    match gl_getattriblocation program "color" with
    | Ok l -> l
    | Error e -> failwith e

  let location_cursor_point_vertex =
    match gl_getattriblocation program_cursor "point" with
    | Ok l -> l
    | Error e -> failwith e

  let init_gl_buffers () =
    gl_enable_vertex_attrib_array location_point_vertex;
    gl_bind_buffer gl_buffer_obj;
    gl_buffer_data b;
    gl_bind_buffer gl_buffer_cursor;
    gl_buffer_data cursor_buffer

  let _ = init_gl_buffers ()

  let draw (editor : Editor.editor) =
    gl_clear_color 1. 1. 1. 1.;
    gl_clear ();
    gl_use_program program;
    draw_editor b cursor_buffer editor;

    gl_bind_buffer gl_buffer_obj;
    gl_vertex_attrib_pointer_float_type ~location:location_point_vertex
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

    gl_vertex_attrib_pointer_float_type ~location:location_color
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;
    gl_buffer_subdata b;

    let buffer_size = get_buffer_size b in
    gl_draw_arrays (buffer_size / _EACH_POINT_FLOAT_AMOUNT);
    reset_buffer b;

    gl_bind_buffer gl_buffer_cursor;
    gl_vertex_attrib_pointer_float_type ~location:location_cursor_point_vertex
      ~stride:_EACH_POINT_FLOAT_AMOUNT_CURSOR ~normalized:false ~start_idx:0;
    gl_buffer_subdata cursor_buffer;

    let buffer_size = get_buffer_size cursor_buffer in
    gl_draw_arrays (buffer_size / _EACH_POINT_FLOAT_AMOUNT_CURSOR);
    reset_buffer cursor_buffer;

    match Sdl.sdl_gl_swapwindow Sdl.w with Ok () -> () | Error e -> failwith e
end
