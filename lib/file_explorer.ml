open Sdl

let files = Files.list_files "."
let file_tree = Files.build_file_tree "."
let flattened_file_tree = Files.flatten_tree file_tree

let print_flattened () =
  List.iter
    (fun (f : Files.file_tree) ->
       match f with
       | Directory { level; name; _ } ->
         Printf.printf "Directory: %s %d\n" name level;
         flush_all ()
       | File f ->
         Printf.printf "File: %s\n" f;
         flush_all ())
    flattened_file_tree
;;

let root_children =
  match file_tree with
  | Directory { children; _ } -> children
  | File _ -> []
;;
