open Freetype

type rope =
  | Leaf of string
  | Node of { left : rope; right : rope; weight : int }

let rec length = function
  | Leaf l -> String.length l
  | Node { weight; right; _ } -> length right + weight

let of_string s = Leaf s

let rec length_of_everything_left = function
  | Leaf _ as leaf -> length leaf
  | Node { right; weight; _ } -> weight + length_of_everything_left right

let concat r1 r2 =
  Node { left = r1; right = r2; weight = length_of_everything_left r1 }

let rec to_string = function
  | Leaf l -> l
  | Node { left; right; _ } -> to_string left ^ to_string right

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
  match r with
  | Leaf _ as ropeLeaf -> (
      let left = substring ropeLeaf ~start:0 ~len:pos in
      let right = substring ropeLeaf ~start:pos ~len:(length ropeLeaf - pos) in
      match (length left, length right) with
      | 0, 0 -> Leaf s
      | _, 0 -> concat left (Leaf s)
      | 0, _ -> concat (Leaf s) right
      | _ -> concat left (concat (Leaf s) right))
  | Node { left; right; _ } ->
      if pos <= length left then concat (insert left pos s) right
      else concat left (insert right (pos - length left) s)

let delete r ~start ~len =
  let before = substring r ~start:0 ~len:start in
  let after =
    substring r ~start:(start + len) ~len:(length r - (start + len))
  in
  concat before after

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
