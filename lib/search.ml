let list_files dir =
  let rec list_files' dir' acc =
    let files = Sys.readdir dir' |> Array.to_list in
    let dirs =
      List.filter (fun file -> Sys.is_directory (dir' ^ "/" ^ file)) files
    in
    files
    @ List.fold_left
        (fun acc' dir'' -> list_files' (dir' ^ "/" ^ dir'') acc')
        acc dirs
  in
  list_files' dir []
