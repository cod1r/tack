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
            height_constraint = Some Min
          ; width_constraint = Some Min
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

let create_textarea_with_line_numbers ?text ~textarea_width ~textarea_height () =
  let container =
    { Ui.default_box with
      height_constraint = Some Min
    ; width_constraint = Some Min
    ; flow = Some Horizontal
    ; clip_content = true
    }
  in
  let line_numbers =
    { Ui.default_box with
      clip_content = true
    ; background_color = 0.4, 0.4, 0.4, 1.
    ; flow = Some Vertical
    ; height_constraint = Some Max
    ; width_constraint = Some Min
    }
  in
  let textarea =
    match text with
    | Some text -> Ui_textarea.create_textarea_box ~text ()
    | None -> Ui_textarea.create_textarea_box ()
  in
  textarea.text_wrap <- false;
  textarea.bbox <- Some { x = 0; y = 0; width = textarea_width; height = textarea_height };
  container.content <- Some (Boxes [ line_numbers; textarea ]);
  container.update <- Some (adjust_textarea_with_line_numbers ~textarea ~line_numbers);
  container
;;
