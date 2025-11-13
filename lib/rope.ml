type rope =
  | Leaf of string
  | Node of { left : rope; right : rope; weight : int }

let fib =
  [|
    1;
    1;
    2;
    3;
    5;
    8;
    13;
    21;
    34;
    55;
    89;
    144;
    233;
    377;
    610;
    987;
    1597;
    2584;
    4181;
    6765;
    10946;
    17711;
    28657;
    46368;
    75025;
    121393;
    196418;
    317811;
    514229;
    832040;
  |]

let rec depth r =
  match r with
  | Leaf _ -> 1
  | Node { left; right; _ } -> 1 + max (depth left) (depth right)

let is_balanced r =
  let depth' = depth r in
  if depth' - 2 > Array.length fib then false
  else
    fib.(depth' + 2)
    <= match r with Leaf l -> String.length l | Node { weight; _ } -> weight

let rec length = function
  | Leaf l -> String.length l
  | Node { weight; right; _ } -> length right + weight

let rec length_of_everything_left = function
  | Leaf _ as leaf -> length leaf
  | Node { right; weight; _ } -> weight + length_of_everything_left right

let concat r1 r2 =
  Node { left = r1; right = r2; weight = length_of_everything_left r1 }

let rec to_string = function
  | Leaf l -> l
  | Node { left; right; _ } -> to_string left ^ to_string right

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

let rec substring r ~start ~len =
  match r with
  | Leaf l ->
      Leaf
        (try String.sub l start len
         with Invalid_argument e ->
           failwith
             (__FUNCTION__ ^ "; start of sub: " ^ string_of_int start
            ^ "; len: " ^ string_of_int len ^ "; str: " ^ l ^ "; " ^ e))
  | Node { left; right; _ } ->
      if start + len <= length left then substring left ~start ~len
      else if start >= length left then
        substring right ~start:(start - length left) ~len
      else
        let left_part = substring left ~start ~len:(length left - start) in
        let right_part =
          substring right ~start:0 ~len:(len - (length left - start))
        in
        concat left_part right_part

let rec insert r pos s =
  let new_rope =
    match r with
    | Leaf _ as ropeLeaf -> (
        let left = substring ropeLeaf ~start:0 ~len:pos in
        let right =
          substring ropeLeaf ~start:pos ~len:(length ropeLeaf - pos)
        in
        match (length left, length right) with
        | 0, 0 -> Leaf s
        | _, 0 -> concat left (Leaf s)
        | 0, _ -> concat (Leaf s) right
        | _ -> concat left (concat (Leaf s) right))
    | Node { left; right; _ } ->
        if pos <= length left then concat (insert left pos s) right
        else concat left (insert right (pos - length left) s)
  in
  if not (is_balanced new_rope) then rebalance new_rope else new_rope

let get_string_from_list_chars list =
  let list = List.rev list in
  Bytes.init (List.length list) (fun i -> List.nth list i) |> String.of_bytes

let of_string s =
  let chunk, split =
    String.fold_left
      (fun (chunk, acc) c ->
        if List.length chunk = 64 then
          ([ c ], get_string_from_list_chars chunk :: acc)
        else (c :: chunk, acc))
      ([], []) s
  in
  let split =
    if List.length chunk > 0 then get_string_from_list_chars chunk :: split
    else split
  in
  let split = List.rev split in
  List.fold_left (fun acc s -> insert acc (length acc) s) (Leaf "") split

let delete r ~start ~len =
  let before = substring r ~start:0 ~len:start in
  let after =
    substring r ~start:(start + len) ~len:(length r - (start + len))
  in
  concat before after

type rope_traversal_info = { x : int; y : int; rope_pos : int }

type closest_information = {
  closest_col : int option;
  upper_y : int;
  closest_vertical_range : (int * int) option;
  closest_rope : int option;
}

type _ traverse_info =
  | Rope_Traversal_Info :
      rope_traversal_info
      -> rope_traversal_info traverse_info
  | Finding_Cursor :
      (rope_traversal_info * closest_information)
      -> (rope_traversal_info * closest_information) traverse_info

let rec traverse_rope : type a.
    rope:rope ->
    handle_result:(a traverse_info -> char -> a traverse_info) ->
    result:a traverse_info ->
    a =
 fun ~(rope : rope)
     ~(handle_result : a traverse_info -> char -> a traverse_info)
     ~(result : a traverse_info) ->
  match rope with
  | Leaf l -> (
      match result with
      | Rope_Traversal_Info r ->
          let acc = ref r in
          let len = String.length l in
          for i = 0 to len - 1 do
            let (Rope_Traversal_Info temp) =
              handle_result (Rope_Traversal_Info !acc) l.[i]
            in
            acc := temp
          done;
          !acc
      | Finding_Cursor r ->
          let acc = ref r in
          let len = String.length l in
          for i = 0 to len - 1 do
            let (Finding_Cursor temp) =
              handle_result (Finding_Cursor !acc) l.[i]
            in
            acc := temp
          done;
          !acc)
  | Node { left; right; _ } ->
      let left_result = traverse_rope ~rope:left ~handle_result ~result in
      let right_result =
        traverse_rope ~rope:right ~handle_result
          ~result:
            (match result with
            | Rope_Traversal_Info _ -> Rope_Traversal_Info left_result
            | Finding_Cursor _ -> Finding_Cursor left_result)
      in
      right_result
