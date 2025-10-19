type render_buffer = (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t

external gl_check_error : unit -> unit = "check_error" "check_error"

external gl_scissor
  :  x:int
  -> y:int
  -> width:int
  -> height:int
  -> unit
  = "gl_scissor" "gl_scissor"

external gl_disable_scissor : unit -> unit = "gl_disable_scissor" "gl_disable_scissor"
external gl_enable_scissor : unit -> unit = "gl_enable_scissor" "gl_enable_scissor"

external set_gl_tex_parameters
  :  unit
  -> unit
  = "set_gl_tex_parameters" "set_gl_tex_parameters"

external set_gl_tex_parameters_ui_text
  :  unit
  -> unit
  = "set_gl_tex_parameters_ui_text" "set_gl_tex_parameters_ui_text"

external gl_uniform_1i
  :  location:int
  -> value:int
  -> unit
  = "gl_uniform_1i" "gl_uniform_1i"

external gl_gen_texture : unit -> int = "gl_gen_texture" "gl_gen_texture"
external gl_bind_texture : texture_id:int -> unit = "gl_bind_texture" "gl_bind_texture"

external gl_teximage_2d
  :  bytes:bytes
  -> width:int
  -> height:int
  -> unit
  = "gl_teximage_2d" "gl_teximage_2d"

external gl_enable_texture_2d
  :  unit
  -> unit
  = "gl_enable_texture_2d" "gl_enable_texture_2d"

external gl_enable_blending : unit -> unit = "gl_enable_blending" "gl_enable_blending"

external gl_clear_color
  :  (float[@unboxed])
  -> (float[@unboxed])
  -> (float[@unboxed])
  -> (float[@unboxed])
  -> unit
  = "gl_clear_color" "gl_clear_color"

external gl_enable_vertex_attrib_array
  :  int
  -> unit
  = "gl_enable_vertex_attrib_array" "gl_enable_vertex_attrib_array"

external gl_use_program : int -> unit = "gl_use_program" [@@noalloc]
external gl_clear : unit -> unit = "gl_clear" "gl_clear"
external gl_gen_one_buffer : unit -> int = "gl_gen_one_buffer" "gl_gen_one_buffer"

external gl_vertex_attrib_pointer_float_type
  :  location:int
  -> size:int
  -> stride:int
  -> normalized:bool
  -> start_idx:int
  -> unit
  = "gl_vertex_attrib_pointer_float_type" "gl_vertex_attrib_pointer_float_type"

external gl_draw_arrays_with_quads
  :  int
  -> unit
  = "gl_draw_arrays_with_quads" "gl_draw_arrays_with_quads"

external gl_draw_arrays : int -> unit = "gl_draw_arrays" "gl_draw_arrays"
external gl_bind_buffer : int -> unit = "gl_bind_buffer" "gl_bind_buffer"

external gl_getuniformlocation
  :  int
  -> string
  -> (int, string) result
  = "gl_getuniformlocation" "gl_getuniformlocation"

external gl_getattriblocation
  :  int
  -> string
  -> (int, string) result
  = "gl_getattriblocation" "gl_getattriblocation"

external gl_create_vertex_shader
  :  unit
  -> (int, string) result
  = "gl_create_vertex_shader" "gl_create_vertex_shader"

external gl_create_fragment_shader
  :  unit
  -> (int, string) result
  = "gl_create_fragment_shader" "gl_create_fragment_shader"

external gl_compileshader : int -> unit = "gl_compileshader" "gl_compileshader"
external gl_linkprogram : int -> unit = "gl_linkprogram" "gl_linkprogram"

external gl_get_shader_info_log
  :  int
  -> string
  = "gl_get_shader_info_log" "gl_get_shader_info_log"

external gl_get_shader_compile_status
  :  int
  -> bool
  = "gl_get_shader_compile_status" "gl_get_shader_compile_status"

external gl_buffer_data_big_array
  :  render_buffer:render_buffer
  -> capacity:int
  -> unit
  = "gl_buffer_data_big_array" "gl_buffer_data_big_array"

external gl_buffer_subdata_big_array
  :  render_buffer:render_buffer
  -> length:int
  -> unit
  = "gl_buffer_subdata_big_array" "gl_buffer_subdata_big_array"

external gl_shader_source : int -> string -> unit = "gl_shader_source" "gl_shader_source"
external gl_attach_shader : int -> int -> unit = "gl_attach_shader" "gl_attach_shader"

external gl_createprogram
  :  unit
  -> (int, string) result
  = "gl_createprogram" "gl_createprogram"

external glew_init : unit -> unit = "glew_init" "glew_init"

external gl_get_viewport
  :  unit
  -> int * int * int * int
  = "gl_get_viewport" "gl_get_viewport"

external gl_set_viewport
  :  int
  -> int
  -> int
  -> int
  -> unit
  = "gl_set_viewport" "gl_set_viewport"

let compile_shaders_and_return_program ~vertex_id ~fragment_id ~vertex_src ~fragment_src =
  gl_shader_source fragment_id fragment_src;
  gl_shader_source vertex_id vertex_src;
  gl_compileshader fragment_id;
  if not (gl_get_shader_compile_status fragment_id)
  then failwith (gl_get_shader_info_log fragment_id);
  gl_compileshader vertex_id;
  if not (gl_get_shader_compile_status vertex_id)
  then failwith (gl_get_shader_info_log vertex_id);
  let p =
    match gl_createprogram () with
    | Ok p -> p
    | Error e -> failwith e
  in
  gl_attach_shader p fragment_id;
  gl_attach_shader p vertex_id;
  gl_linkprogram p;
  p
;;
