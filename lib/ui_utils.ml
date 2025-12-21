let clone_box ~(box : Ui_types.box) =
  let visited = ref [] in
  let rec clone_box' box =
    if List.exists (fun b -> b == box) !visited then
      failwith "Recursive structure detected"
    else visited := box :: !visited ;
    Ui_types.
      { name= box.name
      ; content=
          ( match box.content with
          | Some (Box b) ->
              Some (Box (clone_box' b))
          | Some (Boxes list) ->
              Some (Boxes (List.map (fun b -> clone_box' b) list))
          | Some
              (ScrollContainer
                 { content
                 ; orientation
                 ; other_scrollcontainer
                 ; scroll
                 ; scrollbar_container
                 ; container } ) ->
              Some
                (ScrollContainer
                   { content= clone_box' content
                   ; orientation
                   ; other_scrollcontainer
                   ; scroll= clone_box' scroll
                   ; scrollbar_container= clone_box' scrollbar_container
                   ; container= clone_box' container } )
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
      ; focusable= box.focusable }
  in
  clone_box' box

let direction_to_string ~(orientation : Ui_types.direction) =
  match orientation with
  | Ui_types.Vertical ->
      "Vertical"
  | Horizontal ->
      "Horizontal"

let get_text_wrap_info ~(box : Ui_types.box) ~glyph ~x ~y ~font_info ~text_wrap
    =
  let bbox =
    match box.bbox with
    | Some bbox ->
        bbox
    | None ->
        failwith ("EXPECTED BBOX;" ^ __LOC__)
  in
  let glyph_info = Freetype.get_glyph_info_from_glyph ~glyph ~font_info in
  if x + glyph_info.x_advance > bbox.x + bbox.width && text_wrap then
    ( ~new_x:(bbox.x + glyph_info.x_advance)
    , ~new_y:(y + font_info.font_height)
    , ~wraps:true )
  else (~new_x:(x + glyph_info.x_advance), ~new_y:y, ~wraps:false)
