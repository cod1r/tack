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
    (abs targetx - coord1x, abs targety - coord1y)
  in
  let abs_diffx_coord2, abs_diffy_coord2 =
    (abs targetx - coord2x, abs targety - coord2y)
  in
  if
    abs_diffx_coord1 >= abs_diffx_coord2 && abs_diffy_coord1 >= abs_diffy_coord2
  then coord2
  else coord1

let rec search_closest (cursor_x, cursor_y) rope acc =
  match rope with
  | Leaf l ->
      let rec fold_map acc1 acc2 lst =
        match lst with
        | [] -> acc1
        | (_, g) :: t ->
            let xadv, yadv = g.FreeType.advance in
            let new_advance = (xadv + fst acc2, yadv + snd acc2) in
            fold_map (new_advance :: acc1) new_advance t
      in
      let accumulated = fold_map [] acc l in
      List.fold_left
        (fun (accx, accy) (x, y) ->
          closer_coord (cursor_x, cursor_y) (accx, accy) (x, y))
        (List.hd accumulated) accumulated
  | Node { left; right; _ } ->
      let left = search_closest (cursor_x, cursor_y) left acc in
      let right = search_closest (cursor_x, cursor_y) right acc in
      closer_coord (cursor_x, cursor_y) left right
