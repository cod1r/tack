module FreeType = struct
  external freetype_init : unit -> unit = "freetype_init" "freetype_init"

  external freetype_load_font : unit -> unit
    = "freetype_load_font" "freetype_load_font"

  type freetype_bitmap = {
    rows : int;
    width : int;
    pitch : int;
    buffer : bytes;
  }

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
    glyph_index : int;
  }

  type kerning = int * int

  external freetype_get_kerning : int -> int -> kerning
    = "freetype_get_kerning" "freetype_get_kerning"

  external freetype_load_glyph_letter : char -> freetype_glyph_info
    = "freetype_load_glyph_letter" "freetype_load_glyph_letter"

  external freetype_set_char_size : unit -> unit
    = "freetype_set_char_size" "freetype_set_char_size"

  external freetype_set_pixel_sizes : int -> unit
    = "freetype_set_pixel_sizes" "freetype_set_pixel_sizes"
end

let glyph_infos =
  let startcode, endcode = (32, 126) in
  let rec get_glyph_info char_code acc =
    Printf.printf "%c" (Char.chr char_code);
    print_newline ();
    if char_code > endcode then acc
    else
      let new_glyph_info =
        FreeType.freetype_load_glyph_letter (Char.chr char_code)
      in
      get_glyph_info (succ char_code) ((char_code, new_glyph_info) :: acc)
  in
  get_glyph_info startcode []

let biggest_horiBearingY =
  List.fold_left
    (fun acc (_, g) -> max g.FreeType.metrics.horiBearingY acc)
    0 glyph_infos
