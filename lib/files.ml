let list_files dir =
  let rec list_files' dir' acc =
    let files =
      Sys.readdir dir' |> Array.to_list |> List.map (fun f -> dir' ^ "/" ^ f)
    in
    let dirs = List.filter (fun file -> Sys.is_directory file) files in
    files @ List.fold_left (fun acc' dir'' -> list_files' dir'' acc') acc dirs
  in
  list_files' dir []

type file_tree =
  | Directory of {name: string; children: file_tree list; level: int}
  | File of string

let build_file_tree dir =
  let rec traverse_file_tree level dir =
    let files =
      Sys.readdir dir |> Array.to_list |> List.map (fun f -> dir ^ "/" ^ f)
    in
    let children =
      List.map
        (fun f ->
          if Sys.is_directory f then traverse_file_tree (level + 1) f
          else File f )
        files
    in
    Directory {name= dir; children; level}
  in
  if Sys.is_directory dir then traverse_file_tree 0 dir else File dir

let flatten_tree file_tree =
  let rec fold acc file_tree =
    match file_tree with
    | Directory {children; _} as d ->
        List.fold_left (fun acc child -> fold acc child) (d :: acc) children
    | File _ as f ->
        f :: acc
  in
  fold [] file_tree

let includes_search files search =
  List.filter
    (fun file ->
      let _, was_found =
        String.fold_left
          (fun (idx, was_found) _ ->
            let len_of_search = String.length search
            and len_of_file = String.length file in
            ( idx + 1
            , was_found
              || idx + len_of_search <= len_of_file
                 && String.sub file idx len_of_search = search
                 && len_of_search > 0 ) )
          (0, false) file
      in
      was_found )
    files
