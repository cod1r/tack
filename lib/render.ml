open Freetype
open Sdl
open Opengl
open Editor

module Render = struct
  external get_buffer_size : Opengl.buffer -> int
    = "get_buffer_size" "get_buffer_size"

  external init_buffer : unit -> Opengl.buffer = "init_buffer" "init_buffer"

  external write_to_buffer :
    Opengl.buffer -> FreeType.glyph_info -> int * int -> int -> int -> int
    = "write_to_buffer" "write_to_buffer"
  [@@noalloc]

  let glyph_info_with_char =
    Array.init
      (126 - 32 + 1)
      (fun i -> FreeType.get_ascii_char_glyph FreeType.face (i + 32))

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

  let fragment =
    match gl_create_fragment_shader () with Ok f -> f | Error e -> failwith e

  let vertex =
    match gl_create_vertex_shader () with Ok v -> v | Error e -> failwith e

  let vertex_cursor =
    match gl_create_vertex_shader () with Ok v -> v | Error e -> failwith (e ^ "_CURSOR")

  let fragment_cursor =
    match gl_create_fragment_shader () with Ok f -> f | Error e -> failwith (e ^ "_CURSOR")

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
        String.fold_right
          (fun c acc ->
            let glyph_info_found =
              Array.find_opt (fun (c', _) -> c' = c) glyph_info_with_char
            in
            match glyph_info_found with
            | Some (_, gi) ->
                let x_advance = FreeType.get_x_advance gi
                and font_height = FreeType.get_font_height FreeType.face in
                let processed_acc_x_offset =
                  write_to_buffer buffer gi window_dims acc
                    font_height
                in
                processed_acc_x_offset + x_advance
            | None ->
                Printf.printf "not found";
                print_char c;
                print_newline ();
                acc)
          l offset
    | Rope.Node { left; right; _ } ->
        let left_offset = draw_rope' buffer left offset in
        draw_rope' buffer right left_offset

  let draw_editor (buffer : buffer) rope =
    let _ = draw_rope' buffer rope 0 in
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
    gl_buffer_data b

  let _ = init_gl_buffers ()

  let draw (editor : Editor.editor) =
    (match editor.rope with
    | Some r ->
        draw_editor b r;
        gl_buffer_subdata b
    | None -> ());
    gl_clear_color 1. 1. 1. 1.;
    gl_clear ();
    gl_use_program program;
    gl_bind_buffer gl_buffer_obj;
    gl_vertex_attrib_pointer_float_type location 3 false;
    let buffer_size = get_buffer_size b in
    (* dividing by three here because each point has 3 components (x,y, alpha) *)
    gl_draw_arrays (buffer_size / 3);
    reset_buffer b;
    match Sdl.sdl_gl_swapwindow Sdl.w with Ok () -> () | Error e -> failwith e
end
