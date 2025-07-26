open Freetype
open Sdl
open Opengl
open Editor

let _EACH_POINT_FLOAT_AMOUNT = 6
let _EACH_POINT_FLOAT_AMOUNT_TEXT = 7

let text_vertex_shader =
  {|
  #version 120

  attribute vec2 vertex;
  attribute vec3 color;
  attribute vec2 tex_coord;

  varying vec3 color_frag;
  varying vec2 tex_coord_frag;

  void main() {
    color_frag = color;
    tex_coord_frag = tex_coord;
    gl_Position = vec4(vertex.x, vertex.y, 0.0, 1.0);
  }
  |}

let text_fragment_shader =
  {|
  #version 120

  varying vec2 tex_coord_frag;
  varying vec3 color_frag;

  uniform sampler2D sampler;

  void main() {
    gl_FragColor = vec4(color_frag.r, color_frag.g, color_frag.b, texture2D(sampler, tex_coord_frag).a);
  }
  |}

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

type rope_traversal_info = {
  x : int;
  y : int;
  rope_pos : int;
  line_num : int;
  line_number_placements : (int * int) list;
}

type render_buffer_wrapper = {
  buffer : Opengl.render_buffer;
  mutable length : int;
}

module FileModeRendering = struct
  let get_tex_coords ~(editor : Editor.editor) ~(glyph : char)
      ~(glyph_info : FreeType.glyph_info_) =
    let _, starting_x =
      Array.fold_left
        (fun (found, acc) (c, gi) ->
          if c = glyph || found then (true, acc)
          else (false, acc + gi.FreeType.width))
        (false, 0) editor.config_info.glyph_info_with_char
    in
    let starting_x, ending_x =
      (starting_x, starting_x + glyph_info.FreeType.width - 1)
    in
    let width_float =
      Float.of_int editor.config_info.font_glyph_texture_atlas_info.width
    in
    let height_float =
      Float.of_int editor.config_info.font_glyph_texture_atlas_info.height
    in
    let left = Float.of_int starting_x /. width_float in
    let right = Float.of_int ending_x /. width_float in
    (left, right, 0., Float.of_int glyph_info.rows /. height_float)

  let write_to_text_buffer ~(render_buf_container : render_buffer_wrapper)
      ~(glyph_info : FreeType.glyph_info_) ~x ~y ~window_width ~window_height
      ~(editor : Editor.editor) ~(glyph : char) =
    let x_scaled, y_scaled =
      List.map (fun v -> Float.of_int v) [ x; y ] |> function
      | first :: second :: _ ->
          ( first /. Float.of_int (window_width / 2),
            second /. Float.of_int (window_height / 2) )
      | _ -> failwith "failed to match list"
    in
    let width_scaled =
      (* dividing by 3 because of FT_LOAD_TARGET_LCD *)
      Float.of_int glyph_info.width /. Float.of_int (window_width / 2)
    and height_scaled =
      Float.of_int glyph_info.rows /. Float.of_int (window_height / 2)
    in
    let left, right, top, bottom = get_tex_coords ~editor ~glyph ~glyph_info
    and horiBearing_Y_Scaled =
      Float.of_int glyph_info.horiBearingY /. Float.of_int (window_height / 2)
    and horiBearing_X_Scaled =
      Float.of_int glyph_info.horiBearingX /. Float.of_int (window_width / 2)
    in
    (*
     layout of the values list is:
       vertex x
       vertex y
       r
       g
       b
       texel coord x
       texel coord y

      that is repeated for the 4 points of the quad
   *)
    let values =
      [
        x_scaled +. horiBearing_X_Scaled -. 1.;
        -.(y_scaled +. height_scaled) +. horiBearing_Y_Scaled +. 1.;
        0.;
        0.;
        0.;
        left;
        bottom;
        x_scaled +. horiBearing_X_Scaled -. 1.;
        -.y_scaled +. horiBearing_Y_Scaled +. 1.;
        0.;
        0.;
        0.;
        left;
        top;
        x_scaled +. width_scaled +. horiBearing_X_Scaled -. 1.;
        -.y_scaled +. horiBearing_Y_Scaled +. 1.;
        0.;
        0.;
        0.;
        right;
        top;
        x_scaled +. width_scaled +. horiBearing_X_Scaled -. 1.;
        -.(y_scaled +. height_scaled) +. horiBearing_Y_Scaled +. 1.;
        0.;
        0.;
        0.;
        right;
        bottom;
      ]
    in
    let start = render_buf_container.length in
    List.iteri
      (fun idx v -> render_buf_container.buffer.{idx + start} <- v)
      values;
    render_buf_container.length <- start + List.length values

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
              let x_advance = glyph_info_found.x_advance in
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
                     failwith "TODO: write highlight info into ui buffer")
                   points);
              { acc with x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
        in
        let _ =
          Editor.traverse_rope r fold_fn_for_draw_highlight
            ({
               x = editor.bounds.x;
               y = editor.bounds.y;
               rope_pos = 0;
               line_number_placements = [];
               line_num = 0;
             }
              : rope_traversal_info)
        in
        ()
    | None -> ()

  let draw_line_numbers ~(editor : Editor.editor) ~vertical_scroll_y_offset
      ~window_width ~window_height ~text_buffer ~line_number_placements =
    List.iter
      (fun (line_num, start_y) ->
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
        let y_pos =
          (vertical_scroll_y_offset * editor.config_info.font_height) + start_y
        in
        if
          y_pos >= editor.bounds.y
          && y_pos <= editor.bounds.y + editor.bounds.height
        then
          let curr_x_offset = ref 0 in
          List.iter
            (fun (c', gi) ->
              write_to_text_buffer ~render_buf_container:text_buffer
                ~glyph_info:gi ~x:!curr_x_offset
                ~y:(y_pos + editor.config_info.descender)
                ~window_width ~window_height ~editor ~glyph:c';
              curr_x_offset := !curr_x_offset + gi.FreeType.x_advance)
            glyph_infos)
      line_number_placements

  let draw_cursor ~(editor : Editor.editor) ~(r : Rope.rope) ~cursor_pos
      ~cursor_buffer ~vertical_scroll_y_offset ~window_width ~window_height
      ~digits_widths_summed =
    let fold_fn_draw_cursor (acc : rope_traversal_info) c =
      match c with
      | '\n' ->
          if acc.rope_pos = cursor_pos then
            Printf.eprintf "TODO: write cursor info to ui buffer\n";
          {
            acc with
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
          let x_advance = glyph_info.x_advance and y_pos = acc.y in
          if
            acc.rope_pos = cursor_pos && y_pos >= editor.bounds.y
            && y_pos >= editor.bounds.y
            && y_pos <= editor.bounds.y + editor.bounds.height
            && acc.x >= editor.bounds.x
            && acc.x <= editor.bounds.x + editor.bounds.width
          then Printf.eprintf "TODO: write cursor info to ui buffer\n";
          { acc with x = acc.x + x_advance; rope_pos = acc.rope_pos + 1 }
    in
    let { rope_pos; _ } =
      Editor.traverse_rope r fold_fn_draw_cursor
        {
          line_number_placements = [];
          x = editor.bounds.x + digits_widths_summed;
          y =
            editor.bounds.y
            + (vertical_scroll_y_offset * editor.config_info.font_height);
          rope_pos = 0;
          line_num = 0;
        }
    in
    if rope_pos = cursor_pos then
      Printf.eprintf "TODO: writei cursor info to ui buffer\n"

  let draw_text ~(editor : Editor.editor) ~(rope : Rope.rope) ~text_buffer
      ~window_width ~window_height ~digits_widths_summed
      ~vertical_scroll_y_offset =
    let fold_fn_for_drawing_text (acc : rope_traversal_info) c =
      if c = '\n' then
        {
          x = digits_widths_summed;
          y = acc.y + editor.config_info.font_height;
          rope_pos = acc.rope_pos + 1;
          line_num = acc.line_num + 1;
          line_number_placements =
            (acc.line_num + 1, acc.y + editor.config_info.font_height)
            :: acc.line_number_placements;
        }
      else
        let _, gi =
          Array.find_opt
            (fun (c', _) -> c' = c)
            editor.config_info.glyph_info_with_char
          |> Option.get
        in
        let x_advance = gi.x_advance in
        let wraps = acc.x + x_advance > editor.bounds.x + editor.bounds.width in
        let new_x, new_y =
          if wraps then
            ( digits_widths_summed + x_advance,
              acc.y + editor.config_info.font_height )
          else (acc.x + x_advance, acc.y)
        in
        let y_pos =
          acc.y + (vertical_scroll_y_offset * editor.config_info.font_height)
        in
        (* descender is a negative value *)
        let descender = editor.config_info.descender in
        if
          y_pos <= editor.bounds.y + editor.bounds.height
          && y_pos >= 0
          && Bytes.length gi.bytes > 0
        then
          write_to_text_buffer ~render_buf_container:text_buffer
            ~x:(if wraps then digits_widths_summed else acc.x)
            ~y:
              (y_pos + descender
              + if wraps then editor.config_info.font_height else 0)
            ~window_width ~window_height ~glyph_info:gi ~glyph:c ~editor;
        { acc with rope_pos = acc.rope_pos + 1; x = new_x; y = new_y }
    in
    Editor.traverse_rope rope fold_fn_for_drawing_text
      {
        line_number_placements = [ (1, editor.config_info.font_height) ];
        rope_pos = 0;
        x = editor.bounds.x + digits_widths_summed;
        y = editor.bounds.y + editor.config_info.font_height;
        line_num = 1;
      }
end

module Render = struct
  let better_text_buffer : render_buffer_wrapper =
    {
      buffer =
        Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout
          (3000 * 3000 * _EACH_POINT_FLOAT_AMOUNT);
      length = 0;
    }

  let zero_buffer : render_buffer_wrapper =
    {
      buffer =
        Bigarray.Array1.init Bigarray.Float32 Bigarray.c_layout
          (3000 * 3000 * _EACH_POINT_FLOAT_AMOUNT)
          (fun _ -> 0.);
      length = 0;
    }

  (* 3000x3000 times 2 floats per point *)
  let ui_buffer =
    {
      buffer =
        Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout
          (3000 * 3000 * _EACH_POINT_FLOAT_AMOUNT);
      length = 0;
    }

  let _ = gl_enable_texture_2d ()
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

  let text_vertex_id =
    match gl_create_vertex_shader () with
    | Ok v -> v
    | Error e -> failwith (e ^ "; couldn't create vertex shader for text")

  let text_fragment_id =
    match gl_create_fragment_shader () with
    | Ok v -> v
    | Error e -> failwith (e ^ "; couldn't create vertex shader for text")

  let text_shader_program =
    compile_shaders_and_return_program ~vertex_id:text_vertex_id
      ~fragment_id:text_fragment_id ~vertex_src:text_vertex_shader
      ~fragment_src:text_fragment_shader

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
            let { line_number_placements; _ } =
              FileModeRendering.draw_text ~editor ~rope:r ~window_width
                ~window_height ~digits_widths_summed ~vertical_scroll_y_offset
                ~text_buffer:better_text_buffer
            in
            FileModeRendering.draw_line_numbers ~editor
              ~vertical_scroll_y_offset ~window_width ~window_height
              ~text_buffer:better_text_buffer ~line_number_placements;
            FileModeRendering.draw_highlight ~editor ~r
              ~vertical_scroll_y_offset ~highlight ~window_width ~window_height
              ~highlight_buffer:ui_buffer ~digits_widths_summed;
            FileModeRendering.draw_cursor ~editor ~r ~cursor_buffer:ui_buffer
              ~cursor_pos ~vertical_scroll_y_offset ~window_width ~window_height
              ~digits_widths_summed
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
              let x_advance = gi.x_advance in
              Printf.eprintf "write search to text buffer\n";
              { acc with x = acc.x + x_advance; rope_pos = acc.rope_pos + 1 }
          | None -> failwith "NO GLYPH FOUND BRUH"
        in
        match search_rope with
        | Some r ->
            let _ =
              Editor.traverse_rope r fold_fn
                {
                  rope_pos = 0;
                  x = editor.bounds.x;
                  y = editor.bounds.y;
                  line_num = 0;
                  line_number_placements = [];
                }
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
                          let x_advance = gi'.x_advance in
                          failwith
                            "TODO: implement file search mode functions for \
                             writing search to text buffer";
                          x_offset := !x_offset + x_advance
                      | None -> ())
                    file)
              results
        | None -> ())

  let gl_buffer_obj = gl_gen_one_buffer ()
  let gl_buffer_ui = gl_gen_one_buffer ()
  let gl_buffer_glyph_texture_atlas = gl_gen_texture ()

  let location_point_vertex =
    match gl_getattriblocation program "point_vertex" with
    | Ok l -> l
    | Error e -> failwith e

  let location_color =
    match gl_getattriblocation program "color_attrib" with
    | Ok l -> l
    | Error e -> failwith e

  let vertex_text_location =
    match gl_getattriblocation text_shader_program "vertex" with
    | Ok l -> l
    | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

  let color_text_location =
    match gl_getattriblocation text_shader_program "color" with
    | Ok l -> l
    | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

  let tex_coord_text_location =
    match gl_getattriblocation text_shader_program "tex_coord" with
    | Ok l -> l
    | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

  let sampler_text_location =
    match gl_getuniformlocation text_shader_program "sampler" with
    | Ok l -> l
    | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

  let setup_glyph_texture ~(editor : Editor.editor) =
    gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
    set_gl_tex_parameters ();
    gl_teximage_2d ~bytes:editor.config_info.font_glyph_texture_atlas_info.bytes
      ~width:editor.config_info.font_glyph_texture_atlas_info.width
      ~height:editor.config_info.font_glyph_texture_atlas_info.height

  let () =
    gl_enable_vertex_attrib_array vertex_text_location;
    gl_enable_vertex_attrib_array color_text_location;
    gl_enable_vertex_attrib_array tex_coord_text_location;
    gl_enable_vertex_attrib_array location_point_vertex;
    gl_enable_vertex_attrib_array location_color;
    gl_bind_buffer gl_buffer_obj;
    gl_buffer_data_big_array ~render_buffer:better_text_buffer.buffer
      ~capacity:(Bigarray.Array1.dim better_text_buffer.buffer);
    gl_bind_buffer gl_buffer_ui;
    gl_buffer_data_big_array ~render_buffer:ui_buffer.buffer
      ~capacity:(Bigarray.Array1.dim ui_buffer.buffer)

  let draw (editor : Editor.editor) =
    gl_clear_color 1. 1. 1. 1.;
    gl_clear ();

    draw_editor editor;

    gl_use_program program;

    gl_bind_buffer gl_buffer_ui;

    gl_vertex_attrib_pointer_float_type ~location:location_point_vertex ~size:2
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

    gl_vertex_attrib_pointer_float_type ~location:location_color ~size:4
      ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;

    gl_use_program text_shader_program;

    (* 0 is default texture unit; reminder that sampler is not location but texture unit *)
    gl_uniform_1i ~location:sampler_text_location ~value:0;

    gl_bind_buffer gl_buffer_obj;

    gl_vertex_attrib_pointer_float_type ~location:vertex_text_location ~size:2
      ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:0;

    gl_vertex_attrib_pointer_float_type ~location:color_text_location ~size:3
      ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:2;

    gl_vertex_attrib_pointer_float_type ~location:tex_coord_text_location
      ~size:2 ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false
      ~start_idx:5;

    gl_buffer_subdata_big_array ~render_buffer:better_text_buffer.buffer
      ~length:better_text_buffer.length;

    gl_draw_arrays_with_quads
      (better_text_buffer.length / _EACH_POINT_FLOAT_AMOUNT);

    gl_buffer_subdata_big_array ~render_buffer:zero_buffer.buffer
      ~length:better_text_buffer.length;

    better_text_buffer.length <- 0;

    match Sdl.sdl_gl_swapwindow Sdl.w with Ok () -> () | Error e -> failwith e
end
