external freetype_init: unit -> unit = "freetype_init" "freetype_init"
external freetype_load_font: unit -> unit = "freetype_load_font" "freetype_load_font"
type freetype_bitmap = {
  rows: int;
  width: int;
  pitch: int;
  buffer: bytes;
};;
external freetype_load_glyph_a: unit -> freetype_bitmap = "freetype_load_glyph_a" "freetype_load_glyph_a"
external freetype_set_char_size: unit -> unit = "freetype_set_char_size" "freetype_set_char_size"
