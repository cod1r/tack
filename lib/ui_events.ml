let event_handlers = ref []

let adjust_scroll_container_for_focused_element b new_text_area_information =
  match
    List.find_opt
      (function
        | Ui_types.ScrollContainer {content; _} ->
            content == b
        | _ ->
            failwith "impossible" )
      !Ui.scrollcontainers
  with
  | Some
      (ScrollContainer {scroll; orientation; other_scrollcontainer; content; _})
    -> (
      Ui.adjust_scrollbar_according_to_textarea_text_caret
        ~text_area_info:new_text_area_information ~scroll ~orientation ~content ;
      match other_scrollcontainer with
      | Some {scroll; orientation; content; _} ->
          Ui.adjust_scrollbar_according_to_textarea_text_caret
            ~text_area_info:new_text_area_information ~scroll ~orientation
            ~content
      | _ ->
          () )
  | _ ->
      ()

let pass_evt_to_focused ~(e : Sdl.event) =
  match !Ui.focused_element with
  | Some b -> (
    match b.content with
    | Some (Textarea info) -> (
      match e with
      | Sdl.KeyboardEvt {kbd_evt_type; keysym; _} -> begin
        let char_code = Char.code keysym in
        let ~font_info, .. =
          Ui.TextTextureInfo.get_or_add_font_size_text_texture
            ~font_size:(Option.value b.font_size ~default:Freetype.font_size)
        in
        match b.bbox with
        | Some _ ->
            let new_text_area_information =
              Ui_textarea.handle_kbd_evt ~box:b ~font_info ~char_code ~keysym
                ~kbd_evt_type ~scroll_x_offset:b.scroll_x_offset
                ~text_area_information:info ~scroll_y_offset:b.scroll_y_offset
            in
            adjust_scroll_container_for_focused_element b
              new_text_area_information ;
            b.content <- Some (Textarea new_text_area_information)
        | None ->
            ()
        end
      | Sdl.MouseMotionEvt {x; y; _}
        when Ui.is_within_box ~x ~y ~from_sdl_evt:true ~box:b -> begin
        match b.bbox with
        | Some _ ->
            let ~font_info, .. =
              Ui.TextTextureInfo.get_or_add_font_size_text_texture
                ~font_size:
                  (Option.value b.font_size ~default:Freetype.font_size)
            in
            let new_info =
              Ui_textarea.handle_mouse_motion_evt ~x ~y ~box:b ~font_info
                ~rope:info.text ~text_area_information:info
                ~scroll_y_offset:b.scroll_y_offset
                ~scroll_x_offset:b.scroll_x_offset
            in
            b.content <- Some (Textarea new_info)
        | None ->
            ()
        end
      | Sdl.MouseButtonEvt {mouse_evt_type; x; y; _}
        when Ui.is_within_box ~x ~y ~from_sdl_evt:true ~box:b -> begin
        match b.bbox with
        | Some _ -> (
            let ~font_info, .. =
              Ui.TextTextureInfo.get_or_add_font_size_text_texture
                ~font_size:
                  (Option.value b.font_size ~default:Freetype.font_size)
            in
            match info.text with
            | Some r -> (
                let rope_pos =
                  Ui_textarea.find_closest_rope_pos_for_cursor_on_coords ~box:b
                    ~font_info ~x ~y ~rope:r ~scroll_y_offset:b.scroll_y_offset
                    ~scroll_x_offset:b.scroll_x_offset
                in
                match mouse_evt_type with
                | Mousedown ->
                    b.content <-
                      Some
                        (Textarea
                           { info with
                             holding_mousedown_rope_pos= Some rope_pos
                           ; cursor_pos= Some rope_pos
                           ; highlight_pos= (Some rope_pos, None) } )
                | Mouseup when info.holding_mousedown_rope_pos |> Option.is_some
                  ->
                    b.content <-
                      Some
                        (Textarea {info with holding_mousedown_rope_pos= None})
                | _ ->
                    () )
            | None ->
                () )
        | None ->
            ()
        end
      | Sdl.MouseWheelEvt {x; y; mouseX; mouseY}
        when Ui.is_within_box ~x:mouseX ~y:mouseY ~box:b ~from_sdl_evt:true ->
      begin
        match
          List.find_opt
            (function
              | Ui_types.ScrollContainer {content; _} ->
                  content == b
              | _ ->
                  failwith "impossible" )
            !Ui.scrollcontainers
        with
        | Some
            (ScrollContainer
               { scroll
               ; orientation
               ; other_scrollcontainer
               ; scrollbar_container
               ; _ } ) -> begin
          let adjust_scroll ~scroll ~scrollbar_container = function
            | Ui_types.Vertical -> begin
              match scroll.Ui_types.bbox with
              | Some bbox ->
                  let Ui_types.
                        { y= scrollbar_container_y
                        ; height= scrollbar_container_height
                        ; _ } =
                    Option.get scrollbar_container.Ui_types.bbox
                  in
                  scroll.bbox <-
                    Some
                      { bbox with
                        y=
                          min
                            ( scrollbar_container_y + scrollbar_container_height
                            - bbox.Ui_types.height )
                            (max scrollbar_container_y (bbox.y + -y)) }
              | None ->
                  failwith "SHOULD HAVE BBOX FOR SCROLL"
              end
            | Horizontal -> begin
              match scroll.bbox with
              | Some bbox ->
                  let Ui_types.
                        { x= scrollbar_container_x
                        ; width= scrollbar_container_width
                        ; _ } =
                    Option.get scrollbar_container.bbox
                  in
                  scroll.bbox <-
                    Some
                      { bbox with
                        x=
                          min
                            ( scrollbar_container_x + scrollbar_container_width
                            - bbox.width )
                            (max scrollbar_container_x (bbox.x + x)) }
              | None ->
                  failwith "SHOULD HAVE BBOX FOR SCROLL"
              end
          in
          adjust_scroll ~scroll ~scrollbar_container orientation ;
          match other_scrollcontainer with
          | Some {scroll; orientation; scrollbar_container; _} ->
              adjust_scroll ~scroll ~scrollbar_container orientation
          | _ ->
              ()
          end
        | _ ->
            ()
        end
      | Sdl.TextInputEvt {text; _} ->
          let new_text_area_information =
            Ui_textarea.handle_txt_evt ~text_area_information:info ~text
          in
          adjust_scroll_container_for_focused_element b
            new_text_area_information ;
          b.content <- Some (Textarea new_text_area_information)
      | _ ->
          () )
    | _ ->
        () )
  | None ->
      ()

let check_for_holding ~(e : Sdl.event) =
  match e with
  | Sdl.KeyboardEvt {keysym; kbd_evt_type; _} -> (
      if Char.code keysym = 1073742048 then
        match kbd_evt_type with
        | Keydown ->
            Ui.holding_ctrl := true
        | Keyup ->
            Ui.holding_ctrl := false )
  | Sdl.MouseButtonEvt {mouse_evt_type; x; y; _} -> (
    match mouse_evt_type with
    | Mousedown ->
        Ui.holding_mousedown := `True (~original_x:x, ~original_y:y)
    | Mouseup ->
        Ui.holding_mousedown := `False )
  | _ ->
      ()

let emit_event ~(e : Sdl.event) =
  pass_evt_to_focused ~e ;
  check_for_holding ~e ;
  List.iter
    (fun (box, event_handler) -> event_handler ~b:box ~e)
    !event_handlers

(* the reason that it's a box option is because sometimes there are going to be global event handlers *)
let add_event_handler ~(box : Ui_types.box option)
    ~(event_handler : Ui_types.event_handler_t) =
  if
    not
      (List.exists
         (fun (_, evt_handler) -> evt_handler == event_handler)
         !event_handlers )
  then event_handlers := (box, event_handler) :: !event_handlers

let remove_event_handler ~(box : Ui_types.box) =
  event_handlers :=
    List.filter
      (fun (box', _) -> Option.is_none box' || Option.get box' != box)
      !event_handlers
