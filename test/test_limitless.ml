open OUnit2
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

let concat_test _ =
  let r1 = Limitless.Rope.of_string "Hello " glyph_infos in
  let r2 = Limitless.Rope.of_string "World!" glyph_infos in
  let r3 = Limitless.Rope.concat r1 r2 in
  assert_equal (Limitless.Rope.to_string r3) "Hello World!"

let delete_test _ =
  let r1 = Limitless.Rope.of_string "Hello " glyph_infos in
  let r2 = Limitless.Rope.delete r1 4 1 in
  let s = Limitless.Rope.to_string r2 in
  assert_equal s "Hell "

let length_test _ =
  let r1 = Limitless.Rope.of_string "Hello " glyph_infos in
  let len = Limitless.Rope.length r1 in
  assert_equal len 6

let tests =
  "rope tests" >::: [
    "concat test" >:: concat_test;
    "delete test" >:: delete_test;
    "length test" >:: length_test
  ]

let () =
  run_test_tt_main tests
