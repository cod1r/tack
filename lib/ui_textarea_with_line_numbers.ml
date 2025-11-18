let create_textarea_with_line_numbers ?text ~textarea_width ~textarea_height ()
    =
  let container =
    {
      Ui.default_box with
      height_constraint = Some Min;
      width_constraint = Some Min;
      flow = Some Horizontal;
    }
  in
  let line_numbers =
    {
      Ui.default_box with
      clip_content = true;
      background_color = (0.4, 0.4, 0.4, 1.);
      flow = Some Vertical;
      height_constraint = Some Max;
      width_constraint = Some Min;
    }
  in
  let textarea =
    match text with
    | Some text -> Ui.create_textarea_box ~text ()
    | None -> Ui.create_textarea_box ()
  in
  textarea.bbox <-
    Some { x = 0; y = 0; width = textarea_width; height = textarea_height };
  container.content <- Some (Boxes [ line_numbers; textarea ]);
  Ui.TextAreaWithLineNumbers { line_numbers; container; textarea }

let get_line_nums rope textarea =
  let textarea_bbox = Option.get textarea.Ui.bbox in
  let ~font_info, .. =
    Ui.TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value textarea.font_size ~default:Freetype.font_size)
  in
  let fold_fn_for_line_nums =
   fun acc c ->
    let (Rope.Line_Numbers (rope_traversal_info, list)) = acc in
    match c with
    | '\n' ->
        Rope.Line_Numbers
          ( {
              rope_traversal_info with
              x = textarea_bbox.x;
              y = rope_traversal_info.y + font_info.Freetype.font_height;
            },
            let most_recent_line_number =
              List.find_opt (fun ln -> Option.is_some ln) list
            in
            match most_recent_line_number with
            | Some hd -> Some (Option.get hd + 1) :: list
            | None -> [ Some 1 ] )
    | _ ->
        let ~wraps, ~new_x, ~new_y =
          Ui.get_text_wrap_info ~bbox:textarea_bbox ~glyph:c
            ~x:rope_traversal_info.x ~y:rope_traversal_info.y ~font_info
            ~text_wrap:textarea.text_wrap
        in
        Rope.Line_Numbers
          ( { rope_traversal_info with x = new_x; y = new_y },
            let most_recent_line_number =
              List.find_opt (fun ln -> Option.is_some ln) list
            in
            match most_recent_line_number with
            | Some _ -> if wraps then None :: list else list
            | None -> [ Some 1 ] )
  in
  let _, line_nums =
    Rope.traverse_rope ~rope ~handle_result:fold_fn_for_line_nums
      ~result:
        (Rope.Line_Numbers
           ({ x = textarea_bbox.x; y = textarea_bbox.y; rope_pos = 0 }, []))
  in
  line_nums

let get_line_number_boxes ~rope ~box_containing_textarea =
  let ~font_info, .. =
    Ui.TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:
        (Option.value box_containing_textarea.Ui.font_size
           ~default:Freetype.font_size)
  in
  let line_nums = get_line_nums rope box_containing_textarea in
  let stringified =
    List.map
      (fun ln -> match ln with Some ln -> Int.to_string ln | None -> "")
      line_nums
  in
  Ui.Boxes
    (List.map
       (fun s ->
         {
           Ui.default_box with
           height_constraint = Some Min;
           width_constraint = Some Min;
           horizontal_align = Some Right;
           content =
             Some
               (Ui.Text
                  {
                    string = s;
                    string_width = Ui_utils.calculate_string_width ~s ~font_info;
                  });
         })
       (stringified |> List.rev))

let adjust_textarea_with_line_numbers ~textarea_with_line_numbers =
  match textarea_with_line_numbers with
  | Ui.TextAreaWithLineNumbers { line_numbers; textarea; _ } -> (
      match textarea.content with
      | Some (Ui.Textarea info) -> (
          let rope = info.text in
          match rope with
          | Some rope ->
              line_numbers.content <-
                Some
                  (get_line_number_boxes ~rope ~box_containing_textarea:textarea)
          | None -> ())
      | Some (ScrollContainer { content; _ }) -> (
          match content.content with
          | Some (Textarea info) -> (
              let rope = info.text in
              match rope with
              | Some rope ->
                  line_numbers.content <-
                    Some
                      (get_line_number_boxes ~rope
                         ~box_containing_textarea:content);
                  line_numbers.scroll_y_offset <- content.scroll_y_offset;
                  line_numbers.scroll_x_offset <- content.scroll_x_offset
              | None -> ())
          | _ -> failwith "impossible")
      | _ -> failwith "impossible")
  | _ -> failwith "impossible"
