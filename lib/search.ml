let list_files dir =
  let rec list_files' dir' acc =
    let files =
      Sys.readdir dir' |> Array.to_list |> List.map (fun f -> dir' ^ "/" ^ f)
    in
    let dirs = List.filter (fun file -> Sys.is_directory file) files in
    files @ List.fold_left (fun acc' dir'' -> list_files' dir'' acc') acc dirs
  in
  list_files' dir []

let includes_search files search =
  List.filter
    (fun file ->
      let _, was_found =
        String.fold_left
          (fun (idx, was_found) _ ->
            let len_of_search = String.length search
            and len_of_file = String.length file in
            ( idx + 1,
              was_found
              || idx + len_of_search <= len_of_file
                 && String.sub file idx len_of_search = search
                 && len_of_search > 0 ))
          (0, false) file
      in
      was_found)
    files
