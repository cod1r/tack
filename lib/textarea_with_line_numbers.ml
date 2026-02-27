open Ui_types

let get_line_nums rope textarea =
  let textarea_bbox = Option.get textarea.bbox in
  let ~font_info, .. =
    Ui.TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value textarea.font_size ~default:Freetype.font_size)
  in
  let _, line_nums =
    Rope.traverse_rope
      ~box:textarea
      ~font_info
      ~rope
      ~handle_result:None
      ~result:
        (Rope_types.Line_Numbers
           ({ x = textarea_bbox.x; y = textarea_bbox.y; rope_pos = 0 }, []))
  in
  line_nums
;;

let get_line_number_boxes ~rope ~box_containing_textarea =
  let line_nums = get_line_nums rope box_containing_textarea in
  let stringified =
    List.map
      (function
        | Some ln -> Int.to_string ln
        | None -> "")
      line_nums
  in
  Boxes
    (List.map
       (fun s ->
          { Ui.default_box with
            height_constraint = Some (Content { fallback_size = 0 })
          ; width_constraint = Some (Content { fallback_size = 0 })
          ; horizontal_align = Some Right
          ; content = Some (Text { string = s })
          ; name = Some "LINE_NUM"
          })
       (stringified |> List.rev))
;;

let adjust_textarea_with_line_numbers ~(textarea : box) ~(line_numbers : box) =
  fun () ->
  match textarea.content with
  | Some (Textarea info) ->
    let rope = info.text in
    (match rope with
     | Some rope ->
       line_numbers.content
       <- Some (get_line_number_boxes ~rope ~box_containing_textarea:textarea);
       (* should only be scrolling vertically for line numbers *)
       line_numbers.scroll_y_offset <- textarea.scroll_y_offset
     | None -> ())
  | _ -> failwith "impossible"
;;

let create_textarea_with_line_numbers ?text ?width_constraint ?height_constraint () =
  let container =
    { Ui.default_box with
      height_constraint =
        (match height_constraint with
         | Some constraint' -> Some constraint'
         | None -> Some (Parent { fallback_size = 0 }))
    ; width_constraint =
        (match width_constraint with
         | Some constraint' -> Some constraint'
         | None -> Some (Parent { fallback_size = 0 }))
    ; flow = Some Horizontal
    ; clip_content = true
    }
  in
  let line_numbers =
    { Ui.default_box with
      clip_content = true
    ; background_color = 0.4, 0.4, 0.4, 1.
    ; flow = Some Vertical
    ; height_constraint =
        (match height_constraint with
         | Some constraint' -> Some constraint'
         | None -> Some (Number 200))
    ; width_constraint = Some (Content { fallback_size = 0 })
    ; name = Some "LINE_NUMBERS_CONTAINER"
    }
  in
  let textarea =
    match text with
    | Some text ->
      Ui_textarea.create_textarea_box
        ~text
        (match width_constraint with
         | Some constraint' -> Some constraint'
         | None -> Some (Number 200))
        (match height_constraint with
         | Some constraint' -> Some constraint'
         | None -> Some (Number 200))
        ()
    | None ->
      Ui_textarea.create_textarea_box
        (match width_constraint with
         | Some constraint' -> Some constraint'
         | None -> Some (Number 200))
        (match height_constraint with
         | Some constraint' -> Some constraint'
         | None -> Some (Number 200))
        ()
  in
  textarea.text_wrap <- false;
  textarea.name <- Some "TEXTAREA_BOX";
  container.content <- Some (Boxes [ line_numbers; textarea ]);
  container.update <- Some (adjust_textarea_with_line_numbers ~textarea ~line_numbers);
  container
;;
