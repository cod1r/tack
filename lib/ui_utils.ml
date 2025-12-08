let clone_box ~(box : Ui.box) =
  let visited = ref [] in
  let rec clone_box' box =
    if List.exists (fun b -> b == box) !visited then
      failwith "Recursive structure detected"
    else visited := box :: !visited ;
    Ui.
      { name= box.name
      ; content=
          ( match box.content with
          | Some (Box b) ->
              Some (Box (clone_box' b))
          | Some (Boxes list) ->
              Some (Boxes (List.map (fun b -> clone_box' b) list))
          | Some
              (ScrollContainer {content; orientation; other_scrollcontainer; _})
            ->
              let content = clone_box' content in
              Some
                (Ui_scrollcontainers.create_scrollcontainer ~content
                   ~orientation ~other_scrollcontainer )
          | Some (Text _)
          | Some (Textarea _)
          | Some (TextAreaWithLineNumbers _)
          | None ->
              box.content )
      ; bbox= box.bbox
      ; text_wrap= box.text_wrap
      ; background_color= box.background_color
      ; border= box.border
      ; flow= box.flow
      ; font_size= box.font_size
      ; width_constraint= box.width_constraint
      ; height_constraint= box.height_constraint
      ; clip_content= box.clip_content
      ; position_type= box.position_type
      ; allow_vertical_scroll= box.allow_vertical_scroll
      ; allow_horizontal_scroll= box.allow_horizontal_scroll
      ; horizontal_align= box.horizontal_align
      ; vertical_align= box.vertical_align
      ; on_event= box.on_event
      ; scroll_y_offset= box.scroll_y_offset
      ; scroll_x_offset= box.scroll_x_offset
      ; focusable= box.focusable
      ; batch_writes= box.batch_writes }
  in
  clone_box' box

let direction_to_string ~(orientation : Ui.direction) =
  match orientation with
  | Ui.Vertical ->
      "Vertical"
  | Ui.Horizontal ->
      "Horizontal"

let calculate_string_width ~s ~font_info =
  String.fold_left
    (fun acc c ->
      if c = '\n' || c = '\t' then acc
      else
        let gi = font_info.Freetype.glyph_info_with_char.(Char.code c - 32) in
        acc + gi.Freetype.x_advance )
    0 s
