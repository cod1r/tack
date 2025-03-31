module FreeType = struct
  type ft_face
  type ft_library
  type ft_glyphslot

  external freetype_init : string -> ft_face * ft_library
    = "freetype_init" "freetype_init"

  external freetype_set_char_size : unit -> unit
    = "freetype_set_char_size" "freetype_set_char_size"

  external freetype_set_pixel_sizes : int -> unit
    = "freetype_set_pixel_sizes" "freetype_set_pixel_sizes"

  external freetype_load_glyph_letter : ft_face -> char -> ft_glyphslot
    = "freetype_load_glyph_letter" "freetype_load_glyph_letter"

  let face, library = freetype_init "/System/Library/Fonts/Menlo.ttc"
  let () = freetype_set_pixel_sizes 6
end
