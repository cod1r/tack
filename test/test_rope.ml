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
  let j_amt = 4_000_000 in
  let all_j = List.init j_amt (fun _ -> new_gj) in
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
  assert_bool
    ("time it takes to append " ^ Int.to_string j_amt ^ " ropes/leaves")
    (end' < 0.00005)

let timing_test_traverse_rope _ =
  let gj = List.find (fun (c, _) -> Char.chr c = 'j') glyph_infos in
  let new_gj = (Char.chr (fst gj), snd gj) in
  let j_amt = 4_000_000 in
  let all_j = List.init j_amt (fun _ -> new_gj) in
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
  let rec traverse_rope r =
    match r with
    | Limitless.Rope.Leaf _ -> ()
    | Node { left; right; _ } ->
        traverse_rope left;
        traverse_rope right
  in
  traverse_rope rope;
  let end' = Unix.gettimeofday () -. start in
  assert_bool
    ("time it takes to traverse " ^ Int.to_string j_amt ^ " rope/leaves")
    (end' < 0.5)

let tests =
  "rope tests"
  >::: [
         "concat test" >:: concat_test;
         "delete test" >:: delete_test;
         "length test" >:: length_test;
         "rope concatenation time" >:: timing_test_concatenation;
         "traversing rope tree time" >:: timing_test_traverse_rope;
       ]

let () = run_test_tt_main tests
