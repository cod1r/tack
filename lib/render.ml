open Freetype
open Sdl
open Opengl
open Editor

module Render = struct
  let text_buffer =
    Stubs.init_buffer ~floats_per_point:Opengl._EACH_POINT_FLOAT_AMOUNT_TEXT

  (* 2 pixels wide * 3000 (seems big enough) * 2 floats per pixel *)
  let cursor_buffer =
    Stubs.init_buffer_with_capacity
      (3000 * 3000 * _EACH_POINT_FLOAT_AMOUNT_CURSOR)

  (* 3000x3000 times 2 floats per point *)
  let highlight_buffer =
    Stubs.init_buffer_with_capacity
      (3000 * 3000 * _EACH_POINT_FLOAT_AMOUNT_HIGHLIGHT)

  let vertex_shader =
    {|
  #version 120
  attribute vec2 point_vertex;
  attribute vec4 color_attrib;
  varying vec4 color;
  void main() {
    gl_Position = vec4(point_vertex.x, point_vertex.y, 0.0, 1.0);
    color = color_attrib;
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

  let vertex_highlight_shader =
    {|
    #version 120
    attribute vec2 point;
    void main() {
      gl_Position = vec4(point.x, point.y, 0., 1.);
    }
    |}

  let fragment_highlight_shader =
    {|
    #version 120
    void main() {
      gl_FragColor = vec4(0., 0., 1., 0.5);
    }
    |}

  let _ = gl_enable_blending ()

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

  let vertex_highlight =
    match gl_create_vertex_shader () with Ok v -> v | Error e -> failwith e

  let fragment_highlight =
    match gl_create_fragment_shader () with Ok v -> v | Error e -> failwith e

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

  let program_highlight =
    gl_shader_source fragment_highlight fragment_highlight_shader;
    gl_shader_source vertex_highlight vertex_highlight_shader;
    gl_compileshader fragment_highlight;
    if not (gl_get_shader_compile_status fragment_highlight) then
      failwith (gl_get_shader_info_log fragment_highlight);
    gl_compileshader vertex_highlight;
    if not (gl_get_shader_compile_status vertex_highlight) then
      failwith (gl_get_shader_info_log vertex_highlight);
    let p =
      match gl_createprogram () with Ok p -> p | Error e -> failwith e
    in
    gl_attach_shader p fragment_highlight;
    gl_attach_shader p vertex_highlight;
    gl_linkprogram p;
    p

  let draw_editor (editor : Editor.editor) =
    let window_dims = Sdl.sdl_gl_getdrawablesize () in
    let window_width, window_height = window_dims in
    let fold_fn (acc : unit Editor.rope_traversal_info) c =
      let draw_highlight x_advance =
        match editor.highlight with
        | Some (highlight_start, highlight_end) ->
            if acc.rope_pos >= highlight_start && acc.rope_pos < highlight_end
            then
              let rows = acc.acc_horizontal_x_pos / window_width
              and mod_x = acc.acc_horizontal_x_pos mod window_width in
              let new_x, new_row =
                if (acc.acc_horizontal_x_pos + x_advance) / window_width > rows
                then (0, rows + 1 + editor.vertical_scroll_y_offset)
                else (mod_x, rows + editor.vertical_scroll_y_offset)
              in
              let points =
                [
                  (new_x, (new_row + 1) * editor.config_info.font_height);
                  (new_x, new_row * editor.config_info.font_height);
                  (new_x + x_advance, new_row * editor.config_info.font_height);
                  ( new_x + x_advance,
                    (new_row + 1) * editor.config_info.font_height );
                ]
              in
              List.iter
                (fun (x, y) ->
                  Stubs.write_to_highlight_buffer ~buffer:highlight_buffer ~x ~y
                    ~window_width ~window_height)
                points
        | None -> ()
      in
      if c = '\n' then (
        let div_ans =
          (acc.acc_horizontal_x_pos + window_width) / window_width
        in
        if acc.rope_pos = editor.Editor.cursor_pos then
          Stubs.write_cursor_to_buffer cursor_buffer window_dims
            acc.acc_horizontal_x_pos editor.config_info.font_height
            ~vertical_scroll_y_offset:editor.vertical_scroll_y_offset;
        ({
           acc_horizontal_x_pos = div_ans * window_width;
           rope_pos = acc.rope_pos + 1;
           accumulation = ();
         }
          : unit Editor.rope_traversal_info))
      else
        let glyph_info_found =
          Array.find_opt
            (fun (c', _) -> c' = c)
            editor.config_info.glyph_info_with_char
        in
        match glyph_info_found with
        | Some (_, gi) ->
            let x_advance = Freetype.FreeType.get_x_advance gi in
            draw_highlight x_advance;
            let plus_x_advance =
              (acc.acc_horizontal_x_pos + x_advance) / window_width
            and without_x_advance = acc.acc_horizontal_x_pos / window_width in
            (* y_pos is basically the variable row in the stub code that writes to the text_buffer; adding vertical_scroll_y_offset times window_width to the horizontal_x_pos doesn't work due to the fact that (-window_width + some_x_advance) / window_width will be zero because of integer division;

       also I am using plus_x_advance because that's what row the glyph will be on. using the without_x_advance
       can just give the wrong row
     *)
            let y_pos =
              (plus_x_advance + editor.vertical_scroll_y_offset)
              * editor.config_info.font_height
            in
            if y_pos <= window_height && y_pos >= 0 then
              Stubs.write_to_buffer text_buffer gi ~window_dims
                ~x_offset:acc.acc_horizontal_x_pos
                ~font_height:editor.config_info.font_height
                ~vertical_scroll_y_offset:editor.vertical_scroll_y_offset;
            let processed_acc_x_offset =
              if plus_x_advance > without_x_advance then
                plus_x_advance * window_width
              else acc.acc_horizontal_x_pos
            in
            if acc.rope_pos = editor.Editor.cursor_pos then
              Stubs.write_cursor_to_buffer cursor_buffer window_dims
                processed_acc_x_offset editor.config_info.font_height
                ~vertical_scroll_y_offset:editor.vertical_scroll_y_offset;
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
        ({ acc_horizontal_x_pos = 0; rope_pos = 0; accumulation = () }
          : unit Editor.rope_traversal_info)
    in
    if editor.cursor_pos = Rope.length (editor.rope |> Option.get) then
      Stubs.write_cursor_to_buffer cursor_buffer window_dims
        acc_horizontal_x_pos editor.config_info.font_height
        ~vertical_scroll_y_offset:editor.vertical_scroll_y_offset;
    ()

  let gl_buffer_obj = gl_gen_one_buffer ()
  let gl_buffer_cursor = gl_gen_one_buffer ()
  let gl_buffer_highlight = gl_gen_one_buffer ()

  let location_point_vertex =
    match gl_getattriblocation program "point_vertex" with
    | Ok l -> l
    | Error e -> failwith e

  let location_color =
    match gl_getattriblocation program "color_attrib" with
    | Ok l -> l
    | Error e -> failwith e

  let location_cursor_point_vertex =
    match gl_getattriblocation program_cursor "point" with
    | Ok l -> l
    | Error e -> failwith e

  let location_highlight_point =
    match gl_getattriblocation program_highlight "point" with
    | Ok l -> l
    | Error e -> failwith e

  let () =
    gl_enable_vertex_attrib_array location_point_vertex;
    gl_enable_vertex_attrib_array location_cursor_point_vertex;
    gl_enable_vertex_attrib_array location_color;
    gl_enable_vertex_attrib_array location_highlight_point;
    gl_bind_buffer gl_buffer_obj;
    gl_buffer_data text_buffer;
    gl_bind_buffer gl_buffer_cursor;
    gl_buffer_data cursor_buffer;
    gl_bind_buffer gl_buffer_highlight;
    gl_buffer_data highlight_buffer

  let draw (editor : Editor.editor) =
    gl_clear_color 1. 1. 1. 1.;
    gl_clear ();

    draw_editor editor;

    gl_use_program program_highlight;

    gl_bind_buffer gl_buffer_highlight;
    gl_vertex_attrib_pointer_float_type ~location:location_highlight_point
      ~size:2 ~stride:2 ~normalized:false ~start_idx:0;
    gl_buffer_subdata highlight_buffer;
    let buffer_size = Stubs.get_buffer_size highlight_buffer in
    gl_draw_arrays_with_quads (buffer_size / _EACH_POINT_FLOAT_AMOUNT_HIGHLIGHT);

    Stubs.reset_buffer highlight_buffer;

    gl_use_program program;

    gl_bind_buffer gl_buffer_obj;

    gl_vertex_attrib_pointer_float_type ~location:location_point_vertex ~size:2
      ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:0;

    gl_vertex_attrib_pointer_float_type ~location:location_color ~size:4
      ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:2;

    gl_buffer_subdata text_buffer;
    let buffer_size = Stubs.get_buffer_size text_buffer in

    gl_draw_arrays (buffer_size / _EACH_POINT_FLOAT_AMOUNT_TEXT);

    Stubs.reset_buffer text_buffer;

    gl_use_program program_cursor;

    gl_bind_buffer gl_buffer_cursor;

    gl_vertex_attrib_pointer_float_type ~location:location_cursor_point_vertex
      ~size:2 ~stride:_EACH_POINT_FLOAT_AMOUNT_CURSOR ~normalized:false
      ~start_idx:0;

    gl_buffer_subdata cursor_buffer;

    let buffer_size = Stubs.get_buffer_size cursor_buffer in
    gl_draw_arrays (buffer_size / _EACH_POINT_FLOAT_AMOUNT_CURSOR);
    Stubs.reset_buffer cursor_buffer;

    match Sdl.sdl_gl_swapwindow Sdl.w with Ok () -> () | Error e -> failwith e
end
