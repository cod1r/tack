open Freetype

type rope =
  | Leaf of (char * FreeType.freetype_glyph_info) list
  | Node of { left : rope; right : rope; length : int }

let length = function Leaf l -> List.length l | Node { length; _ } -> length

let of_string s glyph_infos =
  let list_chars = String.fold_left (fun acc c -> c :: acc) [] s in
  Leaf
    (List.map
       (fun c ->
         (c, List.find (fun (c', _) -> Char.chr c' = c) glyph_infos |> snd))
       list_chars)

let concat r1 r2 =
  Node { left = r1; right = r2; length = length r1 + length r2 }

let rec to_string = function
  | Leaf l -> List.fold_left (fun acc (c, _) -> String.make 1 c ^ acc) "" l
  | Node { left; right; _ } -> to_string left ^ to_string right

let rec substring r start len =
  match r with
  | Leaf l ->
      let rec sublist lst endIdx startIdx idx acc =
        if idx = endIdx then acc
        else
          match lst with
          | [] -> acc
          | h :: t ->
              let newlist = if idx >= startIdx then h :: acc else acc in
              sublist t endIdx startIdx (succ idx) newlist
      in
      Leaf (sublist (List.rev l) (start + len) start 0 [])
  | Node { left; right; _ } ->
      if start + len <= length left then substring left start len
      else if start >= length left then
        substring right (start - length left) len
      else
        let left_part = substring left start (length left - start) in
        let right_part = substring right 0 (len - (length left - start)) in
        concat left_part right_part

let rec insert r pos s =
  match r with
  | Leaf _ as ropeLeaf ->
      let left = substring ropeLeaf 0 pos in
      let right = substring ropeLeaf pos (length ropeLeaf - pos) in
      if length left = 0 || length right = 0 then
        failwith "Leaf node cannot contain empty list"
      else concat left (concat (Leaf s) right)
  | Node { left; right; _ } ->
      if pos <= length left then concat (insert left pos s) right
      else concat left (insert right (pos - length left) s)

let delete r start len =
  let before = substring r 0 start in
  let after = substring r (start + len) (length r - (start + len)) in
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
