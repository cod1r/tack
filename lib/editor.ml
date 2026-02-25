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

let editor : editor =
  { files = []
  ; focused_file =
      Some
        { textarea_with_line_numbers =
            Textarea_with_line_numbers.create_textarea_with_line_numbers
              ~text:(open_file "./lib/ui_rendering.ml")
              ~textarea_width:1000
              ~textarea_height:1000
              ()
        ; file_name = "test"
        ; prev_rope = None
        }
  }
;;

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
     | Some _ ->
       let new_textarea_information =
         Ui_textarea.paste_from_clipboard ~text_area_information
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
          let substring =
            Rope.substring rope ~start ~len:(end' - start) |> Rope.to_string
          in
          let new_rope = Some (Rope.delete rope ~start ~len:(end' - start)) in
          let new_undo_list =
            Ui_types.Deletion { string = substring; pos = start }
            ::
            (if
               List.length text_area_information.history.undo_list
               > Ui_textarea.history_length
             then
               List.take
                 Ui_textarea.history_length
                 text_area_information.history.undo_list
             else text_area_information.history.undo_list)
          in
          let new_history = Ui_types.{ undo_list = new_undo_list; redo_list = [] } in
          let new_textarea_information =
            { text_area_information with
              text = new_rope
            ; cursor_pos = Some start
            ; highlight_pos = None, None
            ; history = new_history
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

let undo_action () =
  let info = get_information_from_focused_file () in
  match info with
  | Some (text_area_information, _) ->
    let focused_file = Option.get editor.focused_file in
    (match focused_file.textarea_with_line_numbers.content with
     | Some (Boxes [ _; textarea ]) ->
       let new_textarea_information = Ui_textarea.undo_action text_area_information in
       textarea.content <- Some (Textarea new_textarea_information)
     | _ -> "impossible " ^ __LOC__ |> failwith)
  | None -> ()
;;

let () = Callback.register "undo_function_from_ocaml" undo_action

let redo_action () =
  let info = get_information_from_focused_file () in
  match info with
  | Some (text_area_information, _) ->
    let focused_file = Option.get editor.focused_file in
    (match focused_file.textarea_with_line_numbers.content with
     | Some (Boxes [ _; textarea ]) ->
       let new_textarea_information = Ui_textarea.redo_action text_area_information in
       textarea.content <- Some (Textarea new_textarea_information)
     | _ -> "impossible " ^ __LOC__ |> failwith)
  | None -> ()
;;

let () = Callback.register "redo_function_from_ocaml" redo_action

let place_holder_box_before_any_focused_file =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 1000; height = 1000 }
  ; background_color = 0.5, 0.5, 0.0, 1.
  ; width_constraint = Some (Number 1000)
  ; height_constraint = Some (Number 1000)
  ; name = Some "WTF"
  ; border =
      Some
        { top_thickness = 10
        ; right_thickness = 10
        ; bottom_thickness = 10
        ; left_thickness = 10
        ; top_left_corner_options = { vertical_radius = 20; horizontal_radius = 20 }
        ; top_right_corner_options = { vertical_radius = 20; horizontal_radius = 20 }
        ; bottom_left_corner_options = { vertical_radius = 20; horizontal_radius = 20 }
        ; bottom_right_corner_options = { vertical_radius = 20; horizontal_radius = 20 }
        ; color = 0., 0., 0., 1.
        }
  }
;;

let wrapper =
  { Ui.default_box with
    height_constraint = Some (Parent { fallback_size = 0 })
  ; width_constraint = Some (Parent { fallback_size = 0 })
  ; content = Some (Boxes [ place_holder_box_before_any_focused_file ])
  ; flow = Some Horizontal
  ; name = Some "URMOM"
  }
;;

let editor_view =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 0; height = 0 }
  ; height_constraint = Some (Parent { fallback_size = 0 })
  ; width_constraint = Some (Parent { fallback_size = 0 })
  ; content = Some (Box wrapper)
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
         wrapper.content <- Some (Boxes [ box ]))
;;
