open Freetype

external get_buffer_size : Opengl.buffer -> int
  = "get_buffer_size" "get_buffer_size"

external init_buffer_with_capacity : int -> Opengl.buffer
  = "init_buffer_with_capacity" "init_buffer_with_capacity"

external init_buffer : floats_per_point:int -> Opengl.buffer
  = "init_buffer" "init_buffer"

external write_cursor_to_buffer :
  cursor_buffer:Opengl.buffer ->
  window_dims:int * int ->
  x:int ->
  y:int ->
  cursor_width:int ->
  cursor_height:int ->
  unit = "write_cursor_to_buffer" "write_cursor_to_buffer"

external write_to_highlight_buffer :
  buffer:Opengl.buffer ->
  x:int ->
  y:int ->
  window_width:int ->
  window_height:int ->
  unit = "write_to_highlight_buffer" "write_to_highlight_buffer"

external reset_buffer : Opengl.buffer -> unit = "reset_buffer" "reset_buffer"

external write_mouse_hover_to_highlight_buffer :
  buffer:Opengl.buffer -> window_width:int -> window_height:int -> unit
  = "write_mouse_hover_to_highlight_buffer"
    "write_mouse_hover_to_highlight_buffer"

external write_search_to_text_buffer :
  text_buffer:Opengl.buffer ->
  glyph_info:FreeType.glyph_info_ ->
  x_offset:int ->
  window_width:int ->
  window_height:int ->
  font_height:int ->
  unit = "write_search_to_text_buffer" "write_search_to_text_buffer"

external write_glyph_to_text_buffer_value :
  text_buffer:Opengl.buffer ->
  glyph_info:FreeType.glyph_info_ ->
  x_offset:int ->
  y_offset:int ->
  window_width:int ->
  window_height:int ->
  unit = "write_glyph_to_text_buffer_value" "write_glyph_to_text_buffer_value"
