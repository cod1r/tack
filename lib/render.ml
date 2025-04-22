open Freetype
open Sdl
open Opengl
open Editor

module Render = struct
  external get_buffer_size : Opengl.buffer -> int
    = "get_buffer_size" "get_buffer_size"

  external init_buffer_with_capacity : int -> Opengl.buffer
    = "init_buffer_with_capacity" "init_buffer_with_capacity"

  external init_buffer : unit -> Opengl.buffer = "init_buffer" "init_buffer"

  external write_cursor_to_buffer :
    Opengl.buffer -> int * int -> int -> int -> unit
    = "write_cursor_to_buffer" "write_cursor_to_buffer"

  external write_to_buffer :
    Opengl.buffer -> FreeType.glyph_info -> int * int -> int -> int -> int
    = "write_to_buffer" "write_to_buffer"
  [@@noalloc]

  external reset_buffer : Opengl.buffer -> unit = "reset_buffer" "reset_buffer"

  let vertex_shader =
    {|
  #version 120
  attribute vec3 point_vertex;
  varying float alpha;
  void main() {
    // in our ocaml code, we put a buffer object that has 3 components consecutively.
    // X,Y,Alpha value,X,Y,Alpha value,...
    gl_Position = vec4(point_vertex.x, point_vertex.y, 0.0, 1.0);
    alpha = point_vertex.z;
  }
  |}

  let fragment_shader =
    {|
  #version 120
  varying float alpha;
  void main() {
    gl_FragColor = vec4(0.0, 0.0, 0.0, alpha);
  }
  |}

  let vertex_shader_cursor =
    {|
  #version 120
  attribute vec3 point;
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
  let b = init_buffer ()
  let cursor_buffer = init_buffer_with_capacity (2 * FreeType.font_height * 3)

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

  let rec draw_rope' (buffer : buffer) rope offset =
    let window_dims = Sdl.sdl_gl_getdrawablesize () in
    match rope with
    | Rope.Leaf l ->
        String.fold_left
          (fun acc c ->
            if c = '\n' then
              let window_width, _ = window_dims in
              let div_ans = (acc + window_width) / window_width in
              div_ans * window_width
            else
              let glyph_info_found =
                Array.find_opt
                  (fun (c', _) -> c' = c)
                  Editor.glyph_info_with_char
              in
              match glyph_info_found with
              | Some (_, gi) ->
                  let x_advance = FreeType.get_x_advance gi in
                  let processed_acc_x_offset =
                    write_to_buffer buffer gi window_dims (acc)
                      FreeType.font_height
                  in
                  processed_acc_x_offset + x_advance
              | None ->
                  Printf.printf "not found";
                  print_char c;
                  print_newline ();
                  acc)
          offset l
    | Rope.Node { left; right; _ } ->
        let left_offset = draw_rope' buffer left offset in
        draw_rope' buffer right left_offset

  let draw_cursor (buffer : buffer) (editor : Editor.editor) vertical_scroll_y_offset =
    let window_dims = Sdl.sdl_gl_getdrawablesize () in
    (* this is used to check what glyph/letter matches with the cursor position in the rope *)
    let rec traverse_rope (rope : Rope.rope) (offset : int)
        (rope_position : int) =
      match rope with
      | Leaf l ->
          String.fold_left
            (fun (curr_offset, rp) c ->
              if c = '\n' then (
                let window_width, _ = window_dims in
                let div_ans = (curr_offset + window_width) / window_width in
                if rp = editor.Editor.cursor_pos then
                  write_cursor_to_buffer buffer window_dims curr_offset
                    FreeType.font_height;
                (div_ans * window_width, rp + 1))
              else
                let glyph_info_found =
                  Array.find_opt
                    (fun (c', _) -> c' = c)
                    Editor.glyph_info_with_char
                in
                match glyph_info_found with
                | Some (_, gi) ->
                    let x_advance = FreeType.get_x_advance gi in
                    let window_width, _ = window_dims in
                    let amt_window_widths = curr_offset / window_width
                    and amt_window_widths_plus_1 =
                      (curr_offset + x_advance) / window_width
                    in
                    let processed_x_offset =
                      if amt_window_widths_plus_1 > amt_window_widths then
                        amt_window_widths_plus_1 * window_width
                      else curr_offset
                    in
                    if rp = editor.Editor.cursor_pos then
                      write_cursor_to_buffer buffer window_dims
                        processed_x_offset FreeType.font_height;
                    (processed_x_offset + x_advance, rp + 1)
                | None -> failwith "not found")
            (offset, rope_position) l
      | Node { left; right; _ } ->
          let left_offset, rp = traverse_rope left offset rope_position in
          traverse_rope right left_offset rp
    in
    let last_x_offset, _ = traverse_rope (Option.get editor.Editor.rope) (vertical_scroll_y_offset * fst window_dims) 0 in
    if editor.cursor_pos = Rope.length (editor.rope |> Option.get) then
      write_cursor_to_buffer buffer window_dims last_x_offset
        FreeType.font_height;
    ()

  let draw_rope (buffer : buffer) rope vertical_scroll_y_offset =
    Printf.printf "%d" vertical_scroll_y_offset; print_newline();
    let window_width, _ = Sdl.sdl_gl_getdrawablesize () in
    let _ = draw_rope' buffer rope (vertical_scroll_y_offset * window_width) in
    ()

  let gl_buffer_obj = gl_gen_one_buffer ()
  let gl_buffer_cursor = gl_gen_one_buffer ()

  let location =
    match gl_getattriblocation program "point_vertex" with
    | Ok l -> l
    | Error e -> failwith e

  let init_gl_buffers () =
    gl_enable_vertex_attrib_array location;
    gl_bind_buffer gl_buffer_obj;
    gl_buffer_data b;
    gl_bind_buffer gl_buffer_cursor;
    gl_buffer_data cursor_buffer

  let _ = init_gl_buffers ()

  let draw (editor : Editor.editor) =
    gl_clear_color 1. 1. 1. 1.;
    gl_clear ();
    gl_use_program program;
    (match editor.Editor.rope with
    | Some r ->
        gl_bind_buffer gl_buffer_obj;
        gl_vertex_attrib_pointer_float_type location 3 false;
        draw_rope b r editor.vertical_scroll_y_offset;
        gl_buffer_subdata b;

        let buffer_size = get_buffer_size b in
        (* dividing by three here because each point has 3 components (x,y, alpha) *)
        gl_draw_arrays (buffer_size / 3);
        reset_buffer b;

        gl_bind_buffer gl_buffer_cursor;
        gl_vertex_attrib_pointer_float_type location 3 false;
        draw_cursor cursor_buffer editor editor.vertical_scroll_y_offset;
        gl_buffer_subdata cursor_buffer;

        let buffer_size = get_buffer_size cursor_buffer in
        (* dividing by three here because each point has 3 components (x,y, alpha) *)
        gl_draw_arrays (buffer_size / 3);
        reset_buffer cursor_buffer
    | None -> ());
    match Sdl.sdl_gl_swapwindow Sdl.w with Ok () -> () | Error e -> failwith e
end
