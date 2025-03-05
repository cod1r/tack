open Limitless
(* Example usage *)
let () =
  let r1 = Rope.of_string "Hello " in
  let r2 = Rope.of_string "World!" in
  let r = Rope.concat r1 r2 in
  print_endline (Rope.to_string r);  (* Outputs: Hello World! *)
  print_endline (Rope.substring r 6 5);  (* Outputs: World *)
  let r_inserted = Rope.insert r 5 ", dear" in
  print_endline (Rope.to_string r_inserted); (* Outputs: Hello, dear World! *)
  let r_deleted = Rope.delete r_inserted 5 6 in
  print_endline (Rope.to_string r_deleted); (* Outputs: Hello World! *)
  let r_balanced = Rope.rebalance r_deleted in
  print_endline (Rope.to_string r_balanced)
