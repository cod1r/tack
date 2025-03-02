module FreeType =
  struct
external freetype_init : unit -> unit = "freetype_init" "freetype_init"

external freetype_load_font : unit -> unit
  = "freetype_load_font" "freetype_load_font"

type freetype_bitmap = { rows : int; width : int; pitch : int; buffer : bytes }

type freetype_glyph_metrics = {
  width : int;
  height : int;
  horiBearingX : int;
  horiBearingY : int;
}

type freetype_glyph_info = {
  advance : int * int;
  metrics : freetype_glyph_metrics;
  bitmap : freetype_bitmap;
}

external freetype_load_glyph_letter : char -> freetype_glyph_info
  = "freetype_load_glyph_letter" "freetype_load_glyph_letter"

external freetype_set_char_size : unit -> unit
  = "freetype_set_char_size" "freetype_set_char_size"

external freetype_set_pixel_sizes : int -> unit
  = "freetype_set_pixel_sizes" "freetype_set_pixel_sizes"
  end;;
