open Limitless.Freetype

let () = FreeType.freetype_init ()
let () = FreeType.freetype_load_font ()
let () = FreeType.freetype_set_pixel_sizes 6

let glyph_infos =
  let startcode, endcode = (32, 126) in
  let rec get_glyph_info char_code acc =
    if char_code > endcode then acc
    else
      let new_glyph_info =
        FreeType.freetype_load_glyph_letter (Char.chr char_code)
      in
      get_glyph_info (succ char_code) ((char_code, new_glyph_info) :: acc)
  in
  get_glyph_info startcode []

let _ =
  let r1 = Limitless.Rope.of_string "Hello " glyph_infos in
  let r2 = Limitless.Rope.of_string "World!" glyph_infos in
  let r3 = Limitless.Rope.concat r1 r2 in
  let r4 = Limitless.Rope.delete r3 5 6 in
  let s = Limitless.Rope.to_string r4 in
  Printf.printf "%s" s;
  print_newline ()

let _ =
  let r1 = Limitless.Rope.of_string "Hello " glyph_infos in
  let r2 = Limitless.Rope.delete r1 4 1 in
  let s = Limitless.Rope.to_string r2 in
  Printf.printf "%s" s;
  print_newline ()

let _ =
  let r1 = Limitless.Rope.of_string "Hello " glyph_infos in
  let len = Limitless.Rope.length r1 in
  Printf.printf "%d" len;
  print_newline ()
