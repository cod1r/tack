open Sdl

type file_explorer =
  { mutable opened_directory : Files.file_tree option
  ; mutable opened_directory_rendered : bool
  }

let file_explorer = { opened_directory = None; opened_directory_rendered = false }

let rec find_parent dir file_tree =
  match file_tree with
  | Files.Directory { children; _ } as parent ->
    if List.mem dir children
    then Some parent
    else
      List.find_map
        (fun child ->
           match child with
           | Files.Directory _ -> find_parent dir child
           | _ -> None)
        children
  | File _ -> "file_tree should always be Directory;" ^ __LOC__ |> failwith
;;
