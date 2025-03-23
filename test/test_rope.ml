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

let timing_test_concatenation _ =
  let gj = List.find (fun (c, _) -> Char.chr c = 'j') glyph_infos in
  let new_gj = (Char.chr (fst gj), snd gj) in
  let all_j = List.init 4_000 (fun _ -> new_gj) in
  let start = Unix.gettimeofday () in
  let rope =
    List.fold_left
      (fun acc gj ->
        match acc with
        | Some a -> Some (Limitless.Rope.concat a (Leaf [ gj ]))
        | None -> Some (Leaf [ gj ]))
      None all_j
    |> Option.get
  in
  ();
  let end' = Unix.gettimeofday () -. start in
  Printf.printf "timing_test took: %f. Rope length: %d\n" end'
    (Limitless.Rope.length rope);
  assert_bool "time it takes to append 4_000 ropes/leaves" (end' < 0.00005)

let tests =
  "rope tests"
  >::: [
         "concat test" >:: concat_test;
         "delete test" >:: delete_test;
         "length test" >:: length_test;
         "rope concatenation time" >:: timing_test_concatenation;
       ]

let () = run_test_tt_main tests
