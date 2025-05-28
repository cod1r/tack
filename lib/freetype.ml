module FreeType = struct
  type ft_face
  type ft_library
  type ft_bitmap
  type glyph_info

  type glyph_info_ = {
    horiBearingX : int;
    horiBearingY : int;
    x_advance : int;
    y_advance : int;
    bytes : bytes;
  }

  external get_font_height : ft_face -> int
    = "get_font_height" "get_font_height"

  external get_x_advance : glyph_info -> int = "get_x_advance" "get_x_advance"

  external get_horiBearingX : glyph_info -> int
    = "get_horiBearingX" "get_horiBearingX"

  external get_ascii_char_glyph_info_ : ft_face -> int -> char * glyph_info_
    = "get_ascii_char_glyph_info_" "get_ascii_char_glyph_info_"

  external get_ascii_char_glyph : ft_face -> int -> char * glyph_info
    = "get_ascii_char_glyph" "get_ascii_char_glyph"

  external freetype_init_library : unit -> ft_library
    = "freetype_init_library" "freetype_init_library"

  external freetype_get_face : string -> ft_library -> ft_face
    = "freetype_get_face" "freetype_get_face"

  external freetype_set_char_size : ft_face -> int -> unit
    = "freetype_set_char_size" "freetype_set_char_size"

  external freetype_set_pixel_sizes : ft_face -> int -> unit
    = "freetype_set_pixel_sizes" "freetype_set_pixel_sizes"

  external free_glyph_info : glyph_info -> unit
    = "free_glyph_info" "free_glyph_info"

  external get_descender : ft_face -> int = "get_descender" "get_descender"

  let library = freetype_init_library ()
end
