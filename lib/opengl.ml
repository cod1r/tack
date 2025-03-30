external gl_clear_color :
  (float[@unboxed]) ->
  (float[@unboxed]) ->
  (float[@unboxed]) ->
  (float[@unboxed]) ->
  unit = "gl_clear_color" "gl_clear_color"

external gl_enable_vertex_attrib_array : int -> unit
  = "gl_enable_vertex_attrib_array" "gl_enable_vertex_attrib_array"

external gl_use_program : int -> unit = "gl_use_program" "gl_use_program"
external gl_clear : unit -> unit = "gl_clear" "gl_clear"

external gl_gen_one_buffer : unit -> int
  = "gl_gen_one_buffer" "gl_gen_one_buffer"

external gl_vertex_attrib_pointer_float_type : int -> int -> bool -> unit
  = "gl_vertex_attrib_pointer_float_type" "gl_vertex_attrib_pointer_float_type"

external gl_draw_arrays : int -> unit = "gl_draw_arrays" "gl_draw_arrays"
external gl_bind_buffer : int -> unit = "gl_bind_buffer" "gl_bind_buffer"

external gl_getattriblocation : int -> string -> (int, string) result
  = "gl_getattriblocation" "gl_getattriblocation"

external gl_create_vertex_shader : unit -> (int, string) result
  = "gl_create_vertex_shader" "gl_create_vertex_shader"

external gl_create_fragment_shader : unit -> (int, string) result
  = "gl_create_fragment_shader" "gl_create_fragment_shader"

external gl_compileshader : int -> unit = "gl_compileshader" "gl_compileshader"
external gl_linkprogram : int -> unit = "gl_linkprogram" "gl_linkprogram"

external gl_get_shader_info_log : int -> string
  = "gl_get_shader_info_log" "gl_get_shader_info_log"

external gl_get_shader_compile_status : int -> bool
  = "gl_get_shader_compile_status" "gl_get_shader_compile_status"

external gl_buffer_data :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int ->
  unit = "gl_buffer_data" "gl_buffer_data"

external gl_buffer_subdata :
  (float, Bigarray.float32_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int ->
  int ->
  unit = "gl_buffer_subdata" "gl_buffer_subdata"

external gl_shader_source : int -> string -> unit
  = "gl_shader_source" "gl_shader_source"

external gl_attach_shader : int -> int -> unit
  = "gl_attach_shader" "gl_attach_shader"

external gl_createprogram : unit -> (int, string) result
  = "gl_createprogram" "gl_createprogram"
