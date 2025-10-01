module FreeType = struct
  type ft_face
  type ft_library

  type glyph_info_ = {
    horiBearingX : int;
    horiBearingY : int;
    x_advance : int;
    y_advance : int;
    bytes : bytes;
    width : int;
    rows : int;
  }

  external get_font_height : ft_face -> int
    = "get_font_height" "get_font_height"

  external get_ascii_char_glyph_info_ :
    ft_face -> int -> int -> char * glyph_info_
    = "get_ascii_char_glyph_info_" "get_ascii_char_glyph_info_"

  external freetype_init_library : unit -> ft_library
    = "freetype_init_library" "freetype_init_library"

  external freetype_get_face : string -> ft_library -> ft_face
    = "freetype_get_face" "freetype_get_face"

  external freetype_done_face : ft_face -> unit
    = "freetype_done_face" "freetype_done_face"

  external freetype_set_char_size : ft_face -> int -> unit
    = "freetype_set_char_size" "freetype_set_char_size"

  external freetype_set_pixel_sizes : ft_face -> int -> unit
    = "freetype_set_pixel_sizes" "freetype_set_pixel_sizes"

  external get_descender : ft_face -> int = "get_descender" "get_descender"
  external get_ascender : ft_face -> int = "get_ascender" "get_ascender"

  let config = Config.read_config ()
  let library = freetype_init_library ()

  let face =
    let font_path =
      Yojson.Safe.Util.member "ui_font_path" config
      |> Yojson.Safe.Util.to_string
    in
    freetype_get_face font_path library

  let font_pixel_size =
    Yojson.Safe.Util.member "ui_font_pixel_size" config
    |> Yojson.Safe.Util.to_int
end
