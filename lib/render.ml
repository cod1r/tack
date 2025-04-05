open Freetype
open Sdl
open Opengl
open Editor

module Render = struct
  external init_buffer : unit -> Opengl.buffer = "init_buffer" "init_buffer"

  external write_to_buffer :
    Opengl.buffer -> FreeType.glyph_info -> int -> int -> int -> unit
    = "write_to_buffer" "write_to_buffer"

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

  let _ = gl_enable_blending ()
  let b = init_buffer ()

  let fragment =
    match gl_create_fragment_shader () with Ok f -> f | Error e -> failwith e

  let vertex =
    match gl_create_vertex_shader () with Ok v -> v | Error e -> failwith e

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

  let rec draw_rope' (buffer : buffer) rope offset =
    let window_width, window_height = Sdl.sdl_gl_getdrawablesize () in
    match rope with
    | Rope.Leaf l ->
        String.fold_right
          (fun c acc ->
            let glyph_info_found =
              Array.find_opt (fun (c', _) -> c' = c) glyph_info_with_char
            in
            match glyph_info_found with
            | Some (_, gi) ->
                let x_advance = FreeType.get_x_advance gi in
                write_to_buffer buffer gi window_width window_height
                  (offset + x_advance);
                acc + x_advance
            | None ->
                Printf.printf "not found";
                print_char c;
                print_newline ();
                acc)
          l offset
    | Rope.Node { left; right; _ } ->
        let left_offset = draw_rope' buffer left offset in
        draw_rope' buffer right left_offset

  let draw_rope (buffer : buffer) rope =
    let _ = draw_rope' buffer rope 0 in
    ()

  let gl_buffer_obj = gl_gen_one_buffer ()

  let location =
    match gl_getattriblocation program "point_vertex" with
    | Ok l -> l
    | Error e -> failwith e

  let init_gl_buffers () =
    gl_enable_vertex_attrib_array location;
    gl_bind_buffer gl_buffer_obj;
    gl_buffer_data b;
    gl_vertex_attrib_pointer_float_type location 3 false

  let _ = init_gl_buffers ()

  let draw rope =
    (match rope with
    | Some r ->
        draw_rope b r;
        gl_buffer_subdata b;
        reset_buffer b
    | None -> ());
    gl_clear_color 1. 1. 1. 1.;
    gl_clear ();
    gl_use_program program;
    gl_bind_buffer gl_buffer_obj;
    gl_vertex_attrib_pointer_float_type location 3 false;
    gl_draw_arrays 20_000;
    match Sdl.sdl_gl_swapwindow Sdl.w with Ok () -> () | Error e -> failwith e
end
