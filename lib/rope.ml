open Ui_types
open Rope_types

let fib =
  [| 1
   ; 1
   ; 2
   ; 3
   ; 5
   ; 8
   ; 13
   ; 21
   ; 34
   ; 55
   ; 89
   ; 144
   ; 233
   ; 377
   ; 610
   ; 987
   ; 1597
   ; 2584
   ; 4181
   ; 6765
   ; 10946
   ; 17711
   ; 28657
   ; 46368
   ; 75025
   ; 121393
   ; 196418
   ; 317811
   ; 514229
   ; 832040
  |]
;;

let rec depth r =
  match r with
  | Leaf _ -> 1
  | Node { left; right; _ } -> 1 + max (depth left) (depth right)
;;

let is_balanced r =
  let depth' = depth r in
  if depth' - 2 > Array.length fib
  then false
  else
    fib.(depth' + 2)
    <=
    match r with
    | Leaf l -> String.length l
    | Node { weight; _ } -> weight
;;

let rec length = function
  | Leaf l -> String.length l
  | Node { weight; right; _ } -> length right + weight
;;

let rec length_of_everything_left = function
  | Leaf _ as leaf -> length leaf
  | Node { right; weight; _ } -> weight + length_of_everything_left right
;;

let concat r1 r2 = Node { left = r1; right = r2; weight = length_of_everything_left r1 }

let rec to_string = function
  | Leaf l -> l
  | Node { left; right; _ } -> to_string left ^ to_string right
;;

let rebalance r =
  let rec flatten = function
    | Leaf s -> [ s ]
    | Node { left; right; _ } -> flatten left @ flatten right
  in
  let rec build = function
    | [] -> failwith "Cannot rebalance an empty rope"
    | [ s ] -> Leaf s
    | lst ->
      let mid = List.length lst / 2 in
      let left = build (List.take mid lst) in
      let right = build (List.drop mid lst) in
      concat left right
  in
  build (flatten r)
;;

let rec substring r ~start ~len =
  match r with
  | Leaf l ->
    Leaf
      (try String.sub l start len with
       | Invalid_argument e ->
         failwith
           (__FUNCTION__
            ^ "; start of sub: "
            ^ string_of_int start
            ^ "; len: "
            ^ string_of_int len
            ^ "; str: "
            ^ l
            ^ "; "
            ^ e))
  | Node { left; right; _ } ->
    if start + len <= length left
    then substring left ~start ~len
    else if start >= length left
    then substring right ~start:(start - length left) ~len
    else (
      let left_part = substring left ~start ~len:(length left - start) in
      let right_part = substring right ~start:0 ~len:(len - (length left - start)) in
      concat left_part right_part)
;;

let rec insert r pos s =
  let new_rope =
    match r with
    | Leaf _ as ropeLeaf ->
      let left = substring ropeLeaf ~start:0 ~len:pos in
      let right = substring ropeLeaf ~start:pos ~len:(length ropeLeaf - pos) in
      (match length left, length right with
       | 0, 0 -> Leaf s
       | _, 0 -> concat left (Leaf s)
       | 0, _ -> concat (Leaf s) right
       | _ -> concat left (concat (Leaf s) right))
    | Node { left; right; _ } ->
      if pos <= length left
      then concat (insert left pos s) right
      else concat left (insert right (pos - length left) s)
  in
  if not (is_balanced new_rope) then rebalance new_rope else new_rope
;;

let get_string_from_list_chars list =
  let list = List.rev list in
  Bytes.init (List.length list) (fun i -> List.nth list i) |> String.of_bytes
;;

let of_string s =
  let chunk, split =
    String.fold_left
      (fun (chunk, acc) c ->
         if List.length chunk = 64
         then [ c ], get_string_from_list_chars chunk :: acc
         else c :: chunk, acc)
      ([], [])
      s
  in
  let split =
    if List.length chunk > 0 then get_string_from_list_chars chunk :: split else split
  in
  let split = List.rev split in
  List.fold_left (fun acc s -> insert acc (length acc) s) (Leaf "") split
;;

let delete r ~start ~len =
  let before = substring r ~start:0 ~len:start in
  let after = substring r ~start:(start + len) ~len:(length r - (start + len)) in
  concat before after
;;

let get_pair_col_and_rope_pos
      ~(rope_traversal_info : rope_traversal_info)
      ~closest_info
      ~x
  =
  match closest_info.closest_vertical_range with
  | Some (s, e) ->
    if rope_traversal_info.y = s && closest_info.upper_y = e
    then (
      match closest_info.closest_col with
      | Some closest_col ->
        if abs (closest_col - x) < abs (rope_traversal_info.x - x)
        then closest_info.closest_col, closest_info.closest_rope
        else Some rope_traversal_info.x, Some rope_traversal_info.rope_pos
      | None -> Some rope_traversal_info.x, Some rope_traversal_info.rope_pos)
    else closest_info.closest_col, closest_info.closest_rope
  | None -> None, None
;;

let fold_rope_traversal_info
      (box : box)
      (font_info : Freetype.font_info)
      (rope_traversal_info : rope_traversal_info traverse_info)
      c
  =
  let bbox =
    match box.bbox with
    | Some bbox -> bbox
    | None -> failwith "EXPECTED BOX to have BBOX"
  in
  let (Rope_Traversal_Info acc) = rope_traversal_info in
  match c with
  | '\n' ->
    Rope_Traversal_Info
      { x = bbox.x; y = acc.y + font_info.font_height; rope_pos = acc.rope_pos + 1 }
  | _ ->
    let ~new_x, ~new_y, .. =
      Ui_utils.get_text_wrap_info ~box ~glyph:c ~x:acc.x ~y:acc.y ~font_info
    in
    Rope_Traversal_Info { x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
;;

let fold_line_numbers
      (box : box)
      (font_info : Freetype.font_info)
      (acc : line_number_info traverse_info)
      c
  =
  let bbox =
    match box.bbox with
    | Some bbox -> bbox
    | None -> failwith "EXPECTED BOX to have BBOX"
  in
  let (Line_Numbers (rope_traversal_info, list)) = acc in
  match c with
  | '\n' ->
    Line_Numbers
      ( { x = bbox.x
        ; y = rope_traversal_info.y + font_info.font_height
        ; rope_pos = rope_traversal_info.rope_pos + 1
        }
      , let most_recent_line_number = List.find_opt (fun ln -> Option.is_some ln) list in
        match most_recent_line_number with
        | Some hd -> Some (Option.get hd + 1) :: list
        | None -> [ Some 1 ] )
  | _ ->
    let ~new_x, ~new_y, ~wraps =
      Ui_utils.get_text_wrap_info
        ~box
        ~glyph:c
        ~x:rope_traversal_info.x
        ~y:rope_traversal_info.y
        ~font_info
    in
    Line_Numbers
      ( { x = new_x; y = new_y; rope_pos = rope_traversal_info.rope_pos + 1 }
      , let most_recent_line_number = List.find_opt (fun ln -> Option.is_some ln) list in
        match most_recent_line_number with
        | Some _ -> if wraps then None :: list else list
        | None -> [ Some 1 ] )
;;

let fold_finding_cursor
      (box : box)
      (font_info : Freetype.font_info)
      (acc : find_cursor_info traverse_info)
      c
  =
  let bbox =
    match box.bbox with
    | Some bbox -> bbox
    | None -> failwith "EXPECTED BOX to have BBOX"
  in
  let (Finding_Cursor (rope_traversal_info, closest_info)) = acc in
  let closest_info =
    match closest_info.original_pos with
    | X x ->
      let closest_col, closest_rope =
        get_pair_col_and_rope_pos ~rope_traversal_info ~closest_info ~x
      in
      { closest_info with closest_col; closest_rope }
    | Y y ->
      let closest_vertical_range =
        if y >= rope_traversal_info.y && y <= closest_info.upper_y
        then Some (rope_traversal_info.y, closest_info.upper_y)
        else closest_info.closest_vertical_range
      in
      { closest_info with closest_vertical_range }
  in
  match c with
  | '\n' ->
    Finding_Cursor
      ( { x = bbox.x
        ; y = rope_traversal_info.y + font_info.font_height
        ; rope_pos = rope_traversal_info.rope_pos + 1
        }
      , { closest_info with upper_y = closest_info.upper_y + font_info.font_height } )
  | _ ->
    let ~new_x, ~new_y, .. =
      Ui_utils.get_text_wrap_info
        ~box
        ~glyph:c
        ~x:rope_traversal_info.x
        ~y:rope_traversal_info.y
        ~font_info
    in
    Finding_Cursor
      ( { x = new_x; y = new_y; rope_pos = rope_traversal_info.rope_pos + 1 }
      , { closest_info with upper_y = new_y + font_info.font_height } )
;;

(* scroll_x_offset and scroll_y_offset should not be handled in traverse_rope function *)
let rec traverse_rope
  : type a.
    box:box
    -> font_info:Freetype.font_info
    -> rope:rope
    -> handle_result:(a traverse_info -> char -> unit) option
    -> result:a traverse_info
    -> a
  =
  fun ~box
    ~font_info
    ~(rope : rope)
    ~(handle_result : (a traverse_info -> char -> unit) option)
    ~(result : a traverse_info) ->
  match rope with
  | Leaf l ->
    (match result with
     | Rope_Traversal_Info r ->
       let acc = ref r in
       let len = String.length l in
       for i = 0 to len - 1 do
         (match handle_result with
          | Some handle_result -> handle_result (Rope_Traversal_Info !acc) l.[i]
          | None -> ());
         let (Rope_Traversal_Info temp) =
           fold_rope_traversal_info box font_info (Rope_Traversal_Info !acc) l.[i]
         in
         acc := temp
       done;
       !acc
     | Line_Numbers r ->
       let acc = ref r in
       let len = String.length l in
       for i = 0 to len - 1 do
         (match handle_result with
          | Some handle_result -> handle_result (Line_Numbers !acc) l.[i]
          | None -> ());
         let (Line_Numbers temp) =
           fold_line_numbers box font_info (Line_Numbers !acc) l.[i]
         in
         acc := temp
       done;
       !acc
     | Finding_Cursor r ->
       let acc = ref r in
       let len = String.length l in
       for i = 0 to len - 1 do
         (match handle_result with
          | Some handle_result -> handle_result (Finding_Cursor !acc) l.[i]
          | None -> ());
         let (Finding_Cursor temp) =
           fold_finding_cursor box font_info (Finding_Cursor !acc) l.[i]
         in
         acc := temp
       done;
       !acc)
  | Node { left; right; _ } ->
    let left_result = traverse_rope ~box ~font_info ~rope:left ~handle_result ~result in
    let right_result =
      traverse_rope
        ~box
        ~font_info
        ~rope:right
        ~handle_result
        ~result:
          (match result with
           | Rope_Traversal_Info _ -> Rope_Traversal_Info left_result
           | Finding_Cursor _ -> Finding_Cursor left_result
           | Line_Numbers _ -> Line_Numbers left_result)
    in
    right_result
;;
