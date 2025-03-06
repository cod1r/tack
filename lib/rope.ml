type rope =
  | Leaf of string
  | Node of { left : rope; right : rope; length : int }

(* Get the length of a rope *)
let length = function Leaf s -> String.length s | Node { length; _ } -> length

(* Create a rope from a string *)
let of_string s = Leaf s

(* Concatenate two ropes *)
let concat r1 r2 =
  Node { left = r1; right = r2; length = length r1 + length r2 }

(* Convert a rope to a string *)
let rec to_string = function
  | Leaf s -> s
  | Node { left; right; _ } -> to_string left ^ to_string right

(* Get a substring of a rope *)
let rec substring r start len =
  match r with
  | Leaf s -> String.sub s start len
  | Node { left; right; _ } ->
      if start + len <= length left then substring left start len
      else if start >= length left then
        substring right (start - length left) len
      else
        let left_part = substring left start (length left - start) in
        let right_part = substring right 0 (len - (length left - start)) in
        left_part ^ right_part

(* Insert a string at a given position in the rope *)
let rec insert r pos s =
  match r with
  | Leaf str ->
      let left = String.sub str 0 pos in
      let right = String.sub str pos (String.length str - pos) in
      concat (Leaf left) (concat (Leaf s) (Leaf right))
  | Node { left; right; _ } ->
      if pos <= length left then concat (insert left pos s) right
      else concat left (insert right (pos - length left) s)

(* Delete a substring from the rope *)
let delete r start len =
  let before = substring r 0 start in
  let after = substring r (start + len) (length r - (start + len)) in
  concat (of_string before) (of_string after)

(* Rebalance a rope *)
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
