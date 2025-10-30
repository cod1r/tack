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

external get_font_height : ft_face -> int = "get_font_height" "get_font_height"

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
    Yojson.Safe.Util.member "ui_font_path" config |> Yojson.Safe.Util.to_string
  in
  freetype_get_face font_path library

let font_size =
  Yojson.Safe.Util.member "ui_font_size" config |> Yojson.Safe.Util.to_int

type text_texture_atlas_info = { width : int; height : int; bytes : bytes }

let get_text_texture_atlas_info
    ~(glyph_info_with_char : (char * glyph_info_) Array.t) =
  let face = face in
  let descender = get_descender face in
  let ascender = get_ascender face in
  let widths_summed =
    Array.fold_left
      (fun acc ((_, gi) : char * glyph_info_) -> acc + gi.width)
      0 glyph_info_with_char
  in
  let global_font_height = ascender - descender in
  let bytes_texture_atlas = Bytes.create (widths_summed * global_font_height) in
  (*
       the font glyph texture atlas is all of the glyphs that is loaded
       concatenated into a large bytes array

       widths of glyphs summed * font height
       ex:
         ABCDEF...
     *)
  let current_width = ref 0 in
  for glyph_info_index = 0 to Array.length glyph_info_with_char - 1 do
    let _, glyph_info = glyph_info_with_char.(glyph_info_index) in
    for row = 0 to glyph_info.rows - 1 do
      let slice =
        Bytes.sub glyph_info.bytes (row * glyph_info.width) glyph_info.width
      in
      Bytes.blit slice 0 bytes_texture_atlas
        (!current_width + (widths_summed * row))
        glyph_info.width
    done;
    current_width := !current_width + glyph_info.width
  done;
  {
    (* width and height of text texture *)
    width = widths_summed;
    height = global_font_height;
    bytes = bytes_texture_atlas;
  }

type font_info = {
  glyph_info_with_char : (char * glyph_info_) Array.t;
  font_size : int;
  font_texture_atlas : text_texture_atlas_info;
  font_height : int;
  ascender : int;
  descender : int;
}

let get_new_font_info_with_font_size ~(font_size : int) ~(face : ft_face) =
  let _, height_ratio = Sdl.get_logical_to_opengl_window_dims_ratio () in
  (* scaling font_size appropriately based on ratio between the opengl window in pixels and the sdl window's pixels *)
  let font_size = font_size * height_ratio in
  let glyph_info_with_char =
    Array.init
      (126 - 32 + 1)
      (fun i -> get_ascii_char_glyph_info_ face (i + 32) font_size)
  in
  let font_height = get_font_height face in
  let descender = get_descender face in
  let ascender = get_ascender face in
  let text_texture_atlas_info =
    get_text_texture_atlas_info ~glyph_info_with_char
  in
  {
    glyph_info_with_char;
    font_size;
    font_texture_atlas = text_texture_atlas_info;
    font_height;
    ascender;
    descender;
  }
