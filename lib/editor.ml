type file =
  { textarea_with_line_numbers : Ui_types.box
  ; file_name : string
  ; prev_rope : Rope_types.rope option
  }

type editor =
  { mutable files : file list
  ; mutable focused_file : file option
  }

let open_file file_name =
  Printf.printf "Trying to open %s" file_name;
  print_newline ();
  let file_contents =
    In_channel.with_open_bin file_name (fun ic -> In_channel.input_all ic)
  in
  String.iter
    (fun c ->
       let code = Char.code c in
       if
         (code > Char.code '~' || code < Char.code ' ')
         && code <> Char.code '\n'
         && code <> Char.code '\t'
       then
         failwith
           "only accepting files with ascii between 32-126 inclusive except for tabs and \
            spaces")
    file_contents;
  Rope.of_string file_contents
;;

let config_has_been_modified_during_runtime () =
  let s = Unix.stat ".config.json" in
  Unix.time () -. s.st_mtime < 1.
;;

let editor : editor = { files = []; focused_file = None }

let get_information_from_focused_file () =
  match editor.focused_file with
  | Some { textarea_with_line_numbers; file_name; _ } ->
    (match textarea_with_line_numbers.content with
     | Some (Boxes [ _; textarea ]) ->
       (match textarea.content with
        | Some (Textarea info) -> Some (info, file_name)
        | _ -> failwith "should always have textarea info")
     | _ -> failwith "should always have Boxes [line_numbers; textarea]")
  | None -> None
;;

(* this is called in sdl_menu_bar.m *)
let save_file () =
  let textarea_information = get_information_from_focused_file () in
  match textarea_information with
  | Some ({ text; _ }, file_name) ->
    (match text with
     | Some text ->
       let old_focused_file = Option.get editor.focused_file in
       editor.focused_file <- Some { old_focused_file with prev_rope = Some text };
       let rope_contents = Rope.to_string text in
       (match
          Out_channel.with_open_bin file_name (fun oc ->
            Out_channel.output_string oc rope_contents)
        with
        | exception _ -> print_endline "failed"
        | _ -> print_endline "success")
     | None -> print_endline "text_area_information text is None")
  | None -> ()
;;

let () = Callback.register "save_function_from_ocaml" save_file

let copy_text_from_file () =
  let info = get_information_from_focused_file () in
  match info with
  | Some ({ text; highlight_pos; _ }, _) ->
    (match text with
     | Some rope -> Ui_textarea.copy_into_clipboard ~rope ~highlight_pos
     | None -> ())
  | None -> ()
;;

let () = Callback.register "copy_function_from_ocaml" copy_text_from_file

let paste_text_into_file () =
  let info = get_information_from_focused_file () in
  match info with
  | Some (({ text; _ } as text_area_information), _) ->
    (match text with
     | Some rope ->
       let new_textarea_information =
         Ui_textarea.paste_from_clipboard ~rope ~text_area_information
       in
       let focused_file = Option.get editor.focused_file in
       (match focused_file.textarea_with_line_numbers.content with
        | Some (Boxes [ _; textarea ]) ->
          textarea.content <- Some (Textarea new_textarea_information)
        | _ -> "impossible " ^ __LOC__ |> failwith)
     | None -> ())
  | None -> ()
;;

let () = Callback.register "paste_function_from_ocaml" paste_text_into_file

let cut_text_from_file () =
  let info = get_information_from_focused_file () in
  match info with
  | Some (({ text; highlight_pos; _ } as text_area_information), _) ->
    (match text with
     | Some rope ->
       Ui_textarea.copy_into_clipboard ~rope ~highlight_pos;
       (match highlight_pos with
        | Some start, Some end' when end' - start > 0 ->
          let new_rope = Some (Rope.delete rope ~start ~len:(end' - start)) in
          let new_textarea_information =
            { text_area_information with
              text = new_rope
            ; cursor_pos = Some start
            ; highlight_pos = None, None
            }
          in
          let focused_file = Option.get editor.focused_file in
          (match focused_file.textarea_with_line_numbers.content with
           | Some (Boxes [ _; textarea ]) ->
             textarea.content <- Some (Textarea new_textarea_information)
           | _ -> "impossible " ^ __LOC__ |> failwith)
        | _ -> ())
     | None -> ())
  | None -> ()
;;

let () = Callback.register "cut_function_from_ocaml" cut_text_from_file

(* TODO:

the event handler for what file item box thing the user clicks on
needs to check a centralized box record type that has a "focused directory"

*)
let file_item_box (f : Files.file_tree) =
  let name =
    match f with
    | Directory { name; _ } -> name
    | File f -> f
  in
  { Ui.default_box with
    content = Some (Text { string = name })
  ; width_constraint = Some { constraint_type = Max; fallback_size = 0 }
  ; height_constraint = Some { constraint_type = Max; fallback_size = 0 }
  ; name = Some name
  ; clip_content = true
  }
;;

let root_file_tree = Files.build_file_tree "."

let handle_file_case_for_mouse_btn_evt name =
  let opt = List.find_opt (fun f -> f.file_name = name) editor.files in
  if opt = None
  then (
    let text = open_file name in
    let textarea_with_line_numbers =
      Textarea_with_line_numbers.create_textarea_with_line_numbers
        ~text
        ~width_constraint:{ constraint_type = Max; fallback_size = 100 }
        ~height_constraint:{ constraint_type = Max; fallback_size = 100 }
        ()
    in
    let file_info =
      { textarea_with_line_numbers; file_name = name; prev_rope = Some text }
    in
    (match textarea_with_line_numbers.content with
     | Some (Boxes [ _; textarea ]) -> Ui_globals.set_focused_element ~box:textarea
     | _ ->
       failwith
         "Should always have Boxes [line_numbers; textarea] as content from \
          create_textarea_with_line_numbers");
    editor.files <- file_info :: editor.files;
    editor.focused_file <- Some file_info)
  else (
    let file_info = Option.get opt in
    (match file_info.textarea_with_line_numbers.content with
     | Some (Boxes [ _; textarea ]) -> Ui_globals.set_focused_element ~box:textarea
     | _ ->
       failwith
         "Should always have Boxes [line_numbers; textarea] as content from \
          create_textarea_with_line_numbers");
    editor.focused_file <- Some file_info)
;;

let () = File_explorer.file_explorer.opened_directory <- Some root_file_tree

let file_explorer_view =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; height = 0; width = 300 }
  ; height_constraint = Some { constraint_type = Max; fallback_size = 0 }
  ; content = None
  ; flow = Some Vertical
  ; on_event =
      Some
        (fun ~b ~e ->
          let file_item_boxes =
            match b with
            | Some b ->
              (match b.content with
               | Some (Boxes list) -> list
               | None ->
                 "the update function for this box should've already been called already \
                  to initialize b.content"
                 |> failwith
               | _ ->
                 failwith
                   "should always be getting list of boxes for file_explorer_view content")
            | None ->
              failwith
                "the box for the file_explorer_view should attached to the event handler"
          in
          match e with
          | MouseMotionEvt { x; y; _ } ->
            (match File_explorer.file_explorer.opened_directory with
             | Some (Directory { children; _ }) ->
               List.iteri
                 (fun i _dir_item ->
                    let corresponding_box_item = List.nth file_item_boxes i in
                    if
                      Ui.is_within_box
                        ~x
                        ~y
                        ~box:corresponding_box_item
                        ~from_sdl_evt:true
                    then corresponding_box_item.background_color <- 0.5, 0.5, 0.5, 1.
                    else corresponding_box_item.background_color <- 1., 1., 1., 1.)
                 children
             | Some (File _) -> failwith "opened_directory should never be File"
             | None -> ())
          | MouseButtonEvt { x; y; mouse_evt_type; _ } when mouse_evt_type = Mousedown ->
            (match File_explorer.file_explorer.opened_directory with
             | Some (Directory { children; _ }) ->
               List.iteri
                 (fun i dir_item ->
                    let corresponding_box_item = List.nth file_item_boxes i in
                    if
                      Ui.is_within_box
                        ~x
                        ~y
                        ~box:corresponding_box_item
                        ~from_sdl_evt:true
                    then (
                      match dir_item with
                      | Files.Directory ({ children = dir_children; _ } as dir_info) ->
                        let parent = File_explorer.find_parent dir_item root_file_tree in
                        (match parent with
                         | Some (Directory parent_info) ->
                           File_explorer.file_explorer.opened_directory_rendered <- false;
                           File_explorer.file_explorer.opened_directory
                           <- Some
                                (Directory
                                   { dir_info with
                                     children =
                                       Directory { parent_info with name = ".." }
                                       :: dir_children
                                   })
                         | Some (File _) -> failwith "File is impossible here"
                         | None ->
                           ( (*
                           current issue:
                            if user clicks on ".." the wanted behavior is to go
                            to the parent directory, BUT what if the file system
                            changes and the parent directory doesn't exist anymore?

                            the file explorer should be updated live to reflect
                            file system changes.

                            what if there is a file named ".."?
                           *) ))
                      | File name ->
                        if mouse_evt_type = Mousedown
                        then handle_file_case_for_mouse_btn_evt name))
                 children
             | Some (File _) -> failwith "opened_directory should never be File"
             | None -> ())
          | _ -> ())
  ; allow_vertical_scroll = true
  }
;;

let () =
  file_explorer_view.update
  <- Some
       (fun () ->
         if not File_explorer.file_explorer.opened_directory_rendered
         then (
           match File_explorer.file_explorer.opened_directory with
           | Some (Directory { children; _ }) ->
             file_explorer_view.content <- Some (Boxes (List.map file_item_box children));
             File_explorer.file_explorer.opened_directory_rendered <- true
           | Some (File _) -> "impossible;" ^ __LOC__ |> failwith
           | None -> ()))
;;

let place_holder_box_before_any_focused_file =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 1000; height = 1000 }
  ; background_color = 0.5, 0.5, 0.0, 1.
  ; width_constraint = Some { constraint_type = Max; fallback_size = 1000 }
  ; height_constraint = Some { constraint_type = Max; fallback_size = 1000 }
  }
;;

let text_search =
  { Ui.default_box with
    width_constraint = Some { constraint_type = Max; fallback_size = 0 }
  ; height_constraint = Some { constraint_type = Min; fallback_size = 0 }
  ; bbox = Some { x = 0; y = 0; width = 0; height = 0 }
  ; content =
      Some
        (Boxes
           [ { Ui.default_box with
               content = Some (Text { string = "Text Search" })
             ; width_constraint = Some { constraint_type = Max; fallback_size = 0 }
             ; height_constraint = Some { constraint_type = Min; fallback_size = 100 }
             }
           ; { Ui.default_box with
               content = Some (Textarea Ui.default_text_area_information)
             ; text_wrap = false
             ; bbox = Some { x = 0; y = 0; width = 0; height = 100 }
             ; width_constraint = Some { constraint_type = Max; fallback_size = 0 }
             ; focusable = true
             ; background_color = 0., 0.5, 0.5, 1.
             ; on_event =
                 Some
                   (fun ~b ~e ->
                     match b with
                     | Some b ->
                       (match e with
                        | Sdl.MouseButtonEvt { mouse_evt_type = Mousedown; x; y; _ } ->
                          if Ui.is_within_box ~x ~y ~from_sdl_evt:true ~box:b
                          then Ui_globals.set_focused_element ~box:b
                        | _ -> ())
                     | None -> ())
             }
           ])
  ; flow = Some Vertical
  }
;;

let wrapper =
  { Ui.default_box with
    height_constraint = Some { constraint_type = Max; fallback_size = 0 }
  ; width_constraint = Some { constraint_type = Max; fallback_size = 0 }
  ; content =
      Some (Boxes [ file_explorer_view; place_holder_box_before_any_focused_file ])
  ; flow = Some Horizontal
  }
;;

let editor_view =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 0; height = 0 }
  ; height_constraint = Some { constraint_type = Max; fallback_size = 0 }
  ; width_constraint = Some { constraint_type = Max; fallback_size = 0 }
  ; content = Some (Boxes [ text_search; wrapper ])
  ; flow =
      Some Vertical
      (*
      This is the event handler for handling focus when the user
        clicks on the textarea.
      the reason the event handler for this is on the parent
      component for the editor is because the editor can easily
      set the focus on the `focused_file` property.

      no need for z-index bullshit. *)
  ; on_event =
      Some
        (fun ~b ~e ->
          let _ = b in
          match e with
          | Sdl.MouseButtonEvt { x; y; mouse_evt_type; _ } ->
            (match editor.focused_file with
             | Some { textarea_with_line_numbers; _ } ->
               (match textarea_with_line_numbers.content with
                | Some (Boxes [ _; textarea ]) ->
                  if
                    Ui.is_within_box ~x ~y ~from_sdl_evt:true ~box:textarea
                    && mouse_evt_type = Mousedown
                  then Ui_globals.set_focused_element ~box:textarea
                | _ -> ())
             | None -> ())
          | _ -> ())
  ; name = Some "EDITOR_VIEW"
  }
;;

let () =
  wrapper.update
  <- Some
       (fun () ->
         let box, prev_rope =
           match editor.focused_file with
           | Some { textarea_with_line_numbers; prev_rope; _ } ->
             textarea_with_line_numbers, prev_rope
           | None -> place_holder_box_before_any_focused_file, None
         in
         let textarea_info = get_information_from_focused_file () in
         (match textarea_info with
          | Some (textarea_info, _file_name) ->
            (match prev_rope with
             | Some rope ->
               if rope != Option.get textarea_info.text
               then Sdl.sdl_setwindowtitle "tack (unsaved)"
               else Sdl.sdl_setwindowtitle "tack"
             | None -> ())
          | None -> ());
         wrapper.content <- Some (Boxes [ file_explorer_view; box ]))
;;
