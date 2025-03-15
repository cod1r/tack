open Freetype
open Rope

(* this file should contain types and logic relating to the internals of the editor *)
type editor = { rope : Rope.rope option; cursor_pos : int * int }

(* I need to really think about whether including the y diff and value is needed *)

let closer_coord target coord1 coord2 =
  let targetx, targety = target in
  let coord1x, coord1y = coord1 in
  let coord2x, coord2y = coord2 in
  let abs_diffx_coord1, abs_diffy_coord1 =
    (Float.abs (targetx -. coord1x), Float.abs (targety -. coord1y))
  in
  let abs_diffx_coord2, abs_diffy_coord2 =
    (Float.abs (targetx -. coord2x), Float.abs (targety -. coord2y))
  in
  if
    abs_diffx_coord1 >= abs_diffx_coord2 && abs_diffy_coord1 >= abs_diffy_coord2
  then coord2
  else coord1

let rec search_closest (cursor_x, cursor_y) rope =
  match rope with
  | Leaf l ->
      let rec fold_map acc1 acc2 lst =
        match lst with
        | [] -> acc1
        | (c, g) :: t ->
            let xadv, yadv = g.FreeType.advance in
            Printf.printf "letter: %c" c;
            print_newline ();
            let new_advance =
              (Int.to_float xadv +. fst acc2, Int.to_float yadv +. snd acc2)
            in
            Printf.printf "new adv: %f %f" (fst acc2) (snd acc2);
            print_newline ();
            fold_map (new_advance :: acc1) new_advance t
      in
      let accumulated = fold_map [] (0., 0.) l in
      List.iter
        (fun (fx, fy) ->
          Printf.printf "pos: %f %f" fx fy;
          print_newline ())
        accumulated;
      List.fold_left
        (fun (accx, accy) (x, y) ->
          closer_coord (cursor_x, cursor_y) (accx, accy) (x, y))
        (List.hd accumulated) accumulated
  | Node { left; right; _ } ->
      let left = search_closest (cursor_x, cursor_y) left in
      let right = search_closest (cursor_x, cursor_y) right in
      let actual_right = (fst left +. fst right, snd left +. snd right) in
      closer_coord (cursor_x, cursor_y) left actual_right
