type file =
  { textarea_with_line_numbers : Ui_types.box
  ; file_name : string
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
       if (code > 126 || code < 32) && code <> 10 && code <> 9
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

let default_editor : editor = { files = []; focused_file = None }
let editor : editor = { default_editor with files = [] }

let get_information_from_focused_file () =
  match editor.focused_file with
  | Some { textarea_with_line_numbers; file_name } ->
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

let file_item_box (f : Files.file_tree) =
  let name =
    match f with
    | Directory { name; _ } -> name
    | File f -> f
  in
  { Ui.default_box with
    content = Some (Text { string = name })
  ; width_constraint = Some Min
  ; height_constraint = Some Min
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
             | MouseButtonEvt { x; y; mouse_evt_type; _ } ->
               if
                 Ui.is_within_box ~x ~y ~box:b ~from_sdl_evt:true
                 && mouse_evt_type = Mousedown
                 &&
                 match f with
                 | File _ -> true
                 | _ -> false
               then (
                 let opt = List.find_opt (fun f -> f.file_name = name) editor.files in
                 if opt = None
                 then (
                   let text = open_file name in
                   let textarea_with_line_numbers =
                     Textarea_with_line_numbers.create_textarea_with_line_numbers
                       ~text
                       ~textarea_width:1000
                       ~textarea_height:1000
                       ()
                   in
                   let file_info = { textarea_with_line_numbers; file_name = name } in
                   (match textarea_with_line_numbers.content with
                    | Some (Boxes [ _; textarea ]) ->
                      Ui_globals.set_focused_element ~box:textarea
                    | _ ->
                      failwith
                        "Should always have Boxes [line_numbers; textarea] as content \
                         from create_textarea_with_line_numbers");
                   editor.files <- file_info :: editor.files;
                   editor.focused_file <- Some file_info)
                 else (
                   let file_info = Option.get opt in
                   (match file_info.textarea_with_line_numbers.content with
                    | Some (Boxes [ _; textarea ]) ->
                      Ui_globals.set_focused_element ~box:textarea
                    | _ ->
                      failwith
                        "Should always have Boxes [line_numbers; textarea] as content \
                         from create_textarea_with_line_numbers");
                   editor.focused_file <- Some file_info))
             | _ -> ())
          | None -> ())
  ; name = Some name
  ; clip_content = true
  }
;;

let file_items = List.map (fun f -> file_item_box f) File_explorer.root_children

let file_explorer =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; height = 0; width = 300 }
  ; height_constraint = Some Min
  ; content = Some (Boxes file_items)
  ; flow = Some Vertical
  }
;;

let editor_view =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 2000; height = 0 }
  ; height_constraint = Some Min
  ; content = Some (Boxes [ file_explorer ])
  ; flow = Some Horizontal
  }
;;

let () =
  editor_view.update
  <- Some
       (fun () ->
         editor_view.content
         <- Some
              (Boxes
                 ([ file_explorer ]
                  @
                  match editor.focused_file with
                  | Some { textarea_with_line_numbers; _ } ->
                    [ textarea_with_line_numbers ]
                  | None -> [])))
;;
