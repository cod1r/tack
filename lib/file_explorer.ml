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

let file_item_box (f : Files.file_tree) =
  let name =
    match f with
    | Directory { name; _ } -> name
    | File f -> f
  in
  { Ui.default_box with
    content = Some (Text { string = name })
  ; width_constraint = Some Max
  ; height_constraint = Some Max
  ; on_event =
      Some
        (fun ~b ~e ->
          match b with
          | Some b ->
            (match e with
             | MouseMotionEvt { x; y; _ } ->
               if Ui.is_within_box ~x ~y ~box:b ~from_sdl_evt:true
               then b.background_color <- 0.5, 0.5, 0.5, 1.
               else b.background_color <- 1., 1., 1., 1.
             | _ -> ())
          | None -> ())
  ; name = Some name
  }
;;

let root_children =
  match file_tree with
  | Directory { children; _ } -> children
  | File _ -> []
;;

let file_items = List.map (fun f -> file_item_box f) root_children
