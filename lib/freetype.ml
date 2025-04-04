open Opengl

module FreeType = struct
  type ft_face
  type ft_library
  type ft_bitmap
  type glyph_info

  external get_ascii_char_glyph : ft_face -> int -> char * glyph_info
    = "get_ascii_char_glyph" "get_ascii_char_glyph"

  external freetype_init : string -> ft_face * ft_library
    = "freetype_init" "freetype_init"

  external freetype_set_pixel_sizes : ft_face -> int -> unit
    = "freetype_set_pixel_sizes" "freetype_set_pixel_sizes"

  let face, library = freetype_init "/System/Library/Fonts/Menlo.ttc"
  let () = freetype_set_pixel_sizes face 13
end
