open Freetype
open Sdl
open Opengl
open Editor

let generic_vertex_shader =
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

let generic_fragment_shader =
  {|
  #version 120
  varying vec4 color;
  void main() {
    gl_FragColor = vec4(color.r, color.g, color.b, color.a);
  }
  |}

let compile_shaders_and_return_program ~vertex_id ~fragment_id ~vertex_src
    ~fragment_src =
  gl_shader_source fragment_id fragment_src;
  gl_shader_source vertex_id vertex_src;
  gl_compileshader fragment_id;
  if not (gl_get_shader_compile_status fragment_id) then
    failwith (gl_get_shader_info_log fragment_id);
  gl_compileshader vertex_id;
  if not (gl_get_shader_compile_status vertex_id) then
    failwith (gl_get_shader_info_log vertex_id);
  let p = match gl_createprogram () with Ok p -> p | Error e -> failwith e in
  gl_attach_shader p fragment_id;
  gl_attach_shader p vertex_id;
  gl_linkprogram p;
  p

type rope_traversal_info = { x : int; y : int; rope_pos : int }

type render_buffer_wrapper = {
  text_buf : Opengl.render_buffer;
  mutable length : int;
}

let better_text_buffer : render_buffer_wrapper =
  {
    text_buf =
      Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout
        (3000 * 3000 * _EACH_POINT_FLOAT_AMOUNT);
    length = 0;
  }

let write_to_render_buffer ~(render_buf_container : render_buffer_wrapper)
    ~(glyph_info : FreeType.glyph_info_) ~x ~y ~window_width ~window_height =
  let start = render_buf_container.length in
  let bytes_index = ref start in
  while !bytes_index < start + Bytes.length glyph_info.bytes do
    let first = !bytes_index
    and second = !bytes_index + 1
    and third = !bytes_index + 2 in
    let new_x =
      (Bytes.get glyph_info.bytes (first - start) |> Char.code |> Float.of_int)
      /. 3.
      +. Float.of_int x
      +. Float.of_int glyph_info.horiBearingX
    in
    let new_x = new_x /. Float.of_int (window_width / 2) in
    let new_x = new_x -. 1. in
    let new_y =
      -.((Bytes.get glyph_info.bytes (second - start)
         |> Char.code |> Float.of_int)
        +. Float.of_int y)
      +. Float.of_int glyph_info.horiBearingY
    in
    let new_y = new_y /. Float.of_int (window_height / 2) in
    let new_y = new_y +. 1. in
    let alpha_value =
      Bytes.get glyph_info.bytes (third - start) |> Char.code |> Float.of_int
    in
    render_buf_container.text_buf.{render_buf_container.length} <- new_x;
    render_buf_container.length <- render_buf_container.length + 1;
    render_buf_container.text_buf.{render_buf_container.length} <- new_y;
    render_buf_container.length <- render_buf_container.length + 1;
    render_buf_container.text_buf.{render_buf_container.length} <- 0.;
    render_buf_container.length <- render_buf_container.length + 1;
    render_buf_container.text_buf.{render_buf_container.length} <- 0.;
    render_buf_container.length <- render_buf_container.length + 1;
    render_buf_container.text_buf.{render_buf_container.length} <- 0.;
    render_buf_container.length <- render_buf_container.length + 1;
    render_buf_container.text_buf.{render_buf_container.length} <- alpha_value /. 255.;
    render_buf_container.length <- render_buf_container.length + 1;
    bytes_index := !bytes_index + 3
  done

module FileModeRendering = struct
  let draw_highlight ~(editor : Editor.editor) ~(r : Rope.rope) ~highlight
      ~vertical_scroll_y_offset ~window_width ~window_height ~highlight_buffer
      ~digits_widths_summed =
    match highlight with
    | Some (highlight_start, highlight_end) ->
        let fold_fn_for_draw_highlight (acc : rope_traversal_info) c =
          match c with
          | '\n' -> { acc with y = acc.y + 1; rope_pos = acc.rope_pos + 1 }
          | _ ->
              let _, glyph_info_found =
                Array.find_opt
                  (fun (c', _) -> c' = c)
                  editor.config_info.glyph_info_with_char
                |> Option.get
              in
              let x_advance = FreeType.get_x_advance glyph_info_found in
              let next_y = acc.y + editor.config_info.font_height in
              let new_x, new_y =
                if acc.x + x_advance > editor.bounds.x + editor.bounds.width
                then (editor.bounds.x, next_y)
                else (acc.x + x_advance, acc.y)
              in
              (if
                 acc.rope_pos >= highlight_start && acc.rope_pos < highlight_end
               then
                 let points =
                   [
                     (acc.x + digits_widths_summed, next_y);
                     (acc.x + digits_widths_summed, acc.y);
                     (acc.x + x_advance + digits_widths_summed, acc.y);
                     (acc.x + x_advance + digits_widths_summed, next_y);
                   ]
                 in
                 List.iter
                   (fun (x, y) ->
                     Stubs.write_to_highlight_buffer ~buffer:highlight_buffer ~x
                       ~y:
                         (y
                         + vertical_scroll_y_offset
                           * editor.config_info.font_height)
                       ~window_width ~window_height)
                   points);
              { x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
        in
        let _ =
          Editor.traverse_rope r fold_fn_for_draw_highlight
            ({ x = editor.bounds.x; y = editor.bounds.y; rope_pos = 0 }
              : rope_traversal_info)
        in
        ()
    | None -> ()

  let draw_line_numbers ~(editor : Editor.editor) ~(r : Rope.rope)
      ~vertical_scroll_y_offset ~window_width ~window_height ~text_buffer =
    let fold_fn_for_draw_line_numbers line_num c =
      match c with
      | '\n' ->
          let digits = string_of_int line_num in
          let glyph_infos =
            String.fold_right
              (fun c' acc ->
                (Array.find_opt
                   (fun (c'', _) -> c'' = c')
                   editor.config_info.glyph_info_with_char
                |> Option.get)
                :: acc)
              digits []
          in
          let line_num_with_vert_offset = line_num + vertical_scroll_y_offset in
          (if
             line_num_with_vert_offset * editor.config_info.font_height
             >= editor.bounds.y
             && line_num_with_vert_offset * editor.config_info.font_height
                <= editor.bounds.y + editor.bounds.height
           then
             let curr_x_offset = ref 0 in
             List.iter
               (fun (_, gi) ->
                 (* Stubs.write_glyph_to_text_buffer_value ~text_buffer *)
                 (*   ~glyph_info:gi ~x_offset:!curr_x_offset *)
                 (*   ~y_offset: *)
                 (*     (line_num_with_vert_offset * editor.config_info.font_height *)
                 (*     + editor.config_info.descender) *)
                 (*   ~window_width ~window_height; *)
                 curr_x_offset := !curr_x_offset + FreeType.get_x_advance gi)
               glyph_infos);
          line_num + 1
      | _ -> line_num
    in
    let _ = Editor.traverse_rope r fold_fn_for_draw_line_numbers 1 in
    ()

  let draw_cursor ~(editor : Editor.editor) ~(r : Rope.rope) ~cursor_pos
      ~cursor_buffer ~vertical_scroll_y_offset ~window_width ~window_height
      ~digits_widths_summed =
    let fold_fn_draw_cursor (acc : rope_traversal_info) c =
      match c with
      | '\n' ->
          if acc.rope_pos = cursor_pos then
            Stubs.write_cursor_to_buffer ~cursor_buffer ~cursor_width:3
              ~cursor_height:editor.config_info.font_height
              ~window_dims:(window_width, window_height)
              ~x:acc.x ~y:acc.y;
          {
            x = digits_widths_summed;
            y = acc.y + editor.config_info.font_height;
            rope_pos = acc.rope_pos + 1;
          }
      | _ ->
          let _, glyph_info =
            Array.find_opt
              (fun (c', _) -> c' = c)
              editor.config_info.glyph_info_with_char
            |> Option.get
          in
          let x_advance = FreeType.get_x_advance glyph_info and y_pos = acc.y in
          if
            acc.rope_pos = cursor_pos && y_pos >= editor.bounds.y
            && y_pos >= editor.bounds.y
            && y_pos <= editor.bounds.y + editor.bounds.height
            && acc.x >= editor.bounds.x
            && acc.x <= editor.bounds.x + editor.bounds.width
          then
            Stubs.write_cursor_to_buffer ~cursor_buffer ~cursor_width:3
              ~cursor_height:editor.config_info.font_height
              ~window_dims:(window_width, window_height)
              ~x:acc.x ~y:y_pos;
          { acc with x = acc.x + x_advance; rope_pos = acc.rope_pos + 1 }
    in
    let { rope_pos; x; y } =
      Editor.traverse_rope r fold_fn_draw_cursor
        {
          x = editor.bounds.x + digits_widths_summed;
          y =
            editor.bounds.y
            + (vertical_scroll_y_offset * editor.config_info.font_height);
          rope_pos = 0;
        }
    in
    if rope_pos = cursor_pos then
      Stubs.write_cursor_to_buffer ~cursor_buffer ~cursor_width:3
        ~cursor_height:editor.config_info.font_height
        ~window_dims:(window_width, window_height)
        ~x ~y

  let draw_text ~(editor : Editor.editor) ~(rope : Rope.rope)
      ~(text_buffer : Opengl.buffer) ~window_width ~window_height
      ~digits_widths_summed ~vertical_scroll_y_offset =
    let fold_fn_for_drawing_text (acc : rope_traversal_info) c =
      if c = '\n' then
        {
          x = digits_widths_summed;
          y = acc.y + editor.config_info.font_height;
          rope_pos = acc.rope_pos + 1;
        }
      else
        let _, gi =
          Array.find_opt
            (fun (c', _) -> c' = c)
            editor.config_info.glyph_info_with_char
          |> Option.get
        in
        let x_advance = FreeType.get_x_advance gi in
        let new_x, new_y =
          if acc.x + x_advance > editor.bounds.x + editor.bounds.width then
            (digits_widths_summed, acc.y + editor.config_info.font_height)
          else (acc.x + x_advance, acc.y)
        in
        let y_pos =
          acc.y + (vertical_scroll_y_offset * editor.config_info.font_height)
        in
        (* descender is a negative value *)
        let descender = editor.config_info.descender in
        let _, ogi =
          Array.find_opt
            (fun (c', _) -> c' = c)
            editor.config_info.other_glyph_info_with_char
          |> Option.get
        in
        if
          y_pos <= editor.bounds.y + editor.bounds.height
          && y_pos >= 0
          && Bytes.length ogi.bytes > 0
        then
          (* Stubs.write_glyph_to_text_buffer_value ~text_buffer ~glyph_info:gi *)
          (*   ~x_offset:acc.x ~y_offset:(y_pos + descender) ~window_width *)
          (*   ~window_height *)
          write_to_render_buffer ~render_buf_container:better_text_buffer
            ~x:acc.x ~y:(y_pos + descender) ~window_width ~window_height
            ~glyph_info:ogi;
        { rope_pos = acc.rope_pos + 1; x = new_x; y = new_y }
    in
    let _ =
      Editor.traverse_rope rope fold_fn_for_drawing_text
        {
          rope_pos = 0;
          x = editor.bounds.x + digits_widths_summed;
          y = editor.bounds.y + editor.config_info.font_height;
        }
    in
    ()
end

module Render = struct
  let text_buffer =
    Stubs.init_buffer ~floats_per_point:Opengl._EACH_POINT_FLOAT_AMOUNT

  (* 3000x3000 (seems big enough) * 2 floats per pixel
     We need it pretty big in the case of multiple cursors
   *)
  let cursor_buffer =
    Stubs.init_buffer_with_capacity (3000 * 3000 * _EACH_POINT_FLOAT_AMOUNT)

  (* 3000x3000 times 2 floats per point *)
  let highlight_buffer =
    Stubs.init_buffer_with_capacity (3000 * 3000 * _EACH_POINT_FLOAT_AMOUNT)

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
    compile_shaders_and_return_program ~vertex_id:vertex ~fragment_id:fragment
      ~vertex_src:generic_vertex_shader ~fragment_src:generic_fragment_shader

  (*
        At first, it seems like there could be a write_rope_to_text_buffer function, BUT
        there are specific details like wrapping that I'd like to handle. Maybe there could be
        an abstraction for that specific wrapping behavior, but let's consider that later.
        *)

  let draw_editor (editor : Editor.editor) =
    let window_dims = Sdl.sdl_gl_getdrawablesize () in
    let window_width, window_height = window_dims in
    let current_rope_wrapper =
      List.nth editor.ropes (editor.current_rope_idx |> Option.get)
    in
    match current_rope_wrapper with
    | File { rope; cursor_pos; vertical_scroll_y_offset; highlight; _ } -> (
        match rope with
        | Some r ->
            let lines = Editor.num_lines r in
            let digits_widths_summed =
              Editor.get_digits_widths_summed ~num_lines:lines ~editor
            in
            FileModeRendering.draw_line_numbers ~editor ~r
              ~vertical_scroll_y_offset ~window_width ~window_height
              ~text_buffer;
            FileModeRendering.draw_highlight ~editor ~r
              ~vertical_scroll_y_offset ~highlight ~window_width ~window_height
              ~highlight_buffer ~digits_widths_summed;
            FileModeRendering.draw_cursor ~editor ~r ~cursor_buffer ~cursor_pos
              ~vertical_scroll_y_offset ~window_width ~window_height
              ~digits_widths_summed;
            FileModeRendering.draw_text ~editor ~rope:r ~text_buffer
              ~window_width ~window_height ~digits_widths_summed
              ~vertical_scroll_y_offset
        | None -> ())
    | FileSearch { search_rope; results; _ } -> (
        (* todo -> implement filesearch writing to buffer logic for drawing *)
        let fold_fn (acc : rope_traversal_info) c =
          let found_glyph =
            Array.find_opt
              (fun (c', _) -> c' = c)
              editor.config_info.glyph_info_with_char
          in
          match found_glyph with
          | Some (_, gi) ->
              let x_advance = FreeType.get_x_advance gi in
              Stubs.write_search_to_text_buffer ~text_buffer ~glyph_info:gi
                ~x_offset:acc.x ~window_width ~window_height
                ~font_height:editor.config_info.font_height;
              { acc with x = acc.x + x_advance; rope_pos = acc.rope_pos + 1 }
          | None -> failwith "NO GLYPH FOUND BRUH"
        in
        match search_rope with
        | Some r ->
            let _ =
              Editor.traverse_rope r fold_fn
                { rope_pos = 0; x = editor.bounds.x; y = editor.bounds.y }
            in
            ();

            List.iteri
              (fun idx file ->
                (* adding two to offset the search results past the search row and the window title bar *)
                let row_pos = (idx + 2) * editor.config_info.font_height in
                if row_pos < window_height then
                  (*
                  there are two coordinate systems, the one that is used in ocaml is top left being the origin.
                  The one used in opengl and the c stub code has the center of the window as the origin.
                   *)
                  let x_offset = ref 0 in
                  String.iter
                    (fun c ->
                      let gi =
                        Array.find_opt
                          (fun (c', _) -> c' = c)
                          editor.config_info.glyph_info_with_char
                      in
                      match gi with
                      | Some (_, gi') ->
                          let x_advance = FreeType.get_x_advance gi' in
                          Stubs.write_glyph_to_text_buffer_value ~text_buffer
                            ~glyph_info:gi' ~x_offset:!x_offset
                            ~y_offset:row_pos ~window_width ~window_height;
                          x_offset := !x_offset + x_advance
                      | None -> ())
                    file)
              results
        | None -> ())

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

  let () =
    gl_enable_vertex_attrib_array location_point_vertex;
    gl_enable_vertex_attrib_array location_color;
    gl_bind_buffer gl_buffer_obj;
    (* gl_buffer_data text_buffer; *)
    gl_buffer_data_big_array ~render_buffer:better_text_buffer.text_buf
      ~capacity:(Bigarray.Array1.dim better_text_buffer.text_buf);
    gl_bind_buffer gl_buffer_cursor;
    gl_buffer_data cursor_buffer;
    gl_bind_buffer gl_buffer_highlight;
    gl_buffer_data highlight_buffer

  let draw (editor : Editor.editor) =
    gl_clear_color 1. 1. 1. 1.;
    gl_clear ();

    draw_editor editor;

    gl_use_program program;

    gl_bind_buffer gl_buffer_highlight;

    gl_vertex_attrib_pointer_float_type ~location:location_point_vertex ~size:2
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

    gl_vertex_attrib_pointer_float_type ~location:location_color ~size:4
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;

    gl_buffer_subdata highlight_buffer;

    let buffer_size = Stubs.get_buffer_size highlight_buffer in

    gl_draw_arrays_with_quads (buffer_size / _EACH_POINT_FLOAT_AMOUNT);

    Stubs.reset_buffer highlight_buffer;

    gl_bind_buffer gl_buffer_obj;

    gl_vertex_attrib_pointer_float_type ~location:location_point_vertex ~size:2
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

    gl_vertex_attrib_pointer_float_type ~location:location_color ~size:4
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;

    (* gl_buffer_subdata text_buffer; *)
    (* let buffer_size = Stubs.get_buffer_size text_buffer in *)
    (* gl_draw_arrays (buffer_size / _EACH_POINT_FLOAT_AMOUNT); *)
    gl_buffer_subdata_big_array ~render_buffer:better_text_buffer.text_buf
      ~length:better_text_buffer.length;
    gl_draw_arrays (better_text_buffer.length / _EACH_POINT_FLOAT_AMOUNT);

    better_text_buffer.length <- 0;

    (* Stubs.reset_buffer text_buffer; *)
    gl_bind_buffer gl_buffer_cursor;

    gl_vertex_attrib_pointer_float_type ~location:location_point_vertex ~size:2
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

    gl_vertex_attrib_pointer_float_type ~location:location_color ~size:4
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;

    gl_buffer_subdata cursor_buffer;

    let buffer_size = Stubs.get_buffer_size cursor_buffer in

    gl_draw_arrays (buffer_size / _EACH_POINT_FLOAT_AMOUNT);

    Stubs.reset_buffer cursor_buffer;

    match Sdl.sdl_gl_swapwindow Sdl.w with Ok () -> () | Error e -> failwith e
end
