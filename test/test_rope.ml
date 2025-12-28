open OUnit2
open Tack.Freetype

let concat_test _ =
  let r1 = Tack.Rope.of_string "Hello " in
  let r2 = Tack.Rope.of_string "World!" in
  let r3 = Tack.Rope.concat r1 r2 in
  assert_equal (Tack.Rope.to_string r3) "Hello World!"
;;

let delete_test _ =
  let r1 = Tack.Rope.of_string "Hello " in
  let r2 = Tack.Rope.of_string "World" in
  let r3 = Tack.Rope.concat r1 r2 in
  let r4 = Tack.Rope.delete r3 ~start:4 ~len:1 in
  let s = Tack.Rope.to_string r4 in
  assert_equal ~msg:(s ^ " is not equal to " ^ "Hell World") s "Hell World"
;;

let length_test _ =
  let r1 = Tack.Rope.of_string "Hello " in
  let len = Tack.Rope.length r1 in
  assert_equal len 6
;;

let substring_test _ =
  let r1 = Tack.Rope.of_string "Hello" in
  let r2 = Tack.Rope.of_string "World" in
  let r3 = Tack.Rope.concat r1 r2 in
  let r4 = Tack.Rope.substring r3 ~start:0 ~len:5 in
  assert_bool "substring failed" (Tack.Rope.to_string r4 = "Hello")
;;

let timing_test_concatenation _ =
  let j_amt = 4_000_000 in
  let all_j = List.init j_amt (fun _ -> "j") in
  let start = Unix.gettimeofday () in
  let rope =
    List.fold_left
      (fun acc gj ->
         match acc with
         | Some a -> Some (Tack.Rope.concat a (Leaf gj))
         | None -> Some (Leaf "j"))
      None
      all_j
    |> Option.get
  in
  ();
  let end' = Unix.gettimeofday () -. start in
  Printf.printf "timing_test took: %f. Rope length: %d\n" end' (Tack.Rope.length rope);
  assert_bool
    ("time it takes to append " ^ Int.to_string j_amt ^ " ropes/leaves")
    (end' < 0.6)
;;

let timing_test_traverse_rope _ =
  let j_amt = 4_000_000 in
  let all_j = List.init j_amt (fun _ -> "j") in
  let start = Unix.gettimeofday () in
  let rope =
    List.fold_left
      (fun acc gj ->
         match acc with
         | Some a -> Some (Tack.Rope.concat a (Leaf gj))
         | None -> Some (Leaf gj))
      None
      all_j
    |> Option.get
  in
  ();
  let rec traverse_rope r =
    match r with
    | Tack.Rope_types.Leaf _ -> ()
    | Node { left; right; _ } ->
      traverse_rope left;
      traverse_rope right
  in
  traverse_rope rope;
  let end' = Unix.gettimeofday () -. start in
  assert_bool
    ("time it takes to traverse " ^ Int.to_string j_amt ^ " rope/leaves")
    (end' < 0.5)
;;

let insertion_rope_test _ =
  let r1 = Tack.Rope.of_string "Hello, Ho"
  and name = " Jason" in
  let r2 = Tack.Rope.insert r1 6 name in
  let str_result = Tack.Rope.to_string r2 in
  assert_bool
    ("insertion rope result failed; got: " ^ str_result)
    (str_result = "Hello, Jason Ho");
  let r3 = Tack.Rope.insert r1 9 name in
  let str_result = Tack.Rope.to_string r3 in
  assert_bool
    ("insertion rope result failed 2; got: " ^ str_result)
    (str_result = "Hello, Ho Jason");
  let r4 = Tack.Rope.insert r1 0 name in
  let str_result = Tack.Rope.to_string r4 in
  assert_bool
    ("insertion rope result failed 3; got: " ^ str_result)
    (str_result = " JasonHello, Ho")
;;

let tests =
  "rope tests"
  >::: [ "delete test" >:: delete_test
       ; "concat test" >:: concat_test
       ; "length test" >:: length_test
       ; "substring test" >:: substring_test
       ; "rope concatenation time" >:: timing_test_concatenation
       ; "traversing rope tree time" >:: timing_test_traverse_rope
       ; "rope insertion test" >:: insertion_rope_test
       ]
;;

let () = run_test_tt_main tests
