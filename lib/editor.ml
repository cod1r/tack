open Freetype
open Rope

module Editor = struct
  type editor = { rope : Rope.rope option; cursor_pos : int }
end

external get_proper_x_offset_value : int -> FreeType.glyph_info -> int -> int
  = "get_proper_x_offset_value" "get_proper_x_offset_value"
