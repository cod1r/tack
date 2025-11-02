open Sdl

let event_handlers = ref []

let pass_evt_to_focused ~(e : Sdl.event) =
  match !Ui.focused_element with
  | Some b -> (
      match b.content with
      | Some (Textarea info) -> (
          match e with
          | Sdl.KeyboardEvt { kbd_evt_type; keysym; _ } -> (
              let char_code = Char.code keysym in
              let ~font_info, .. =
                Ui.TextTextureInfo.get_or_add_font_size_text_texture
                  ~font_size:
                    (Option.value b.font_size ~default:Freetype.font_size)
              in
              match b.bbox with
              | Some _ ->
                  let new_text_area_information =
                    Ui_textarea.handle_kbd_evt
                      ~bbox:(Option.value b.bbox ~default:Ui.default_bbox)
                      ~font_info ~char_code ~keysym ~kbd_evt_type
                      ~text_area_information:info
                      ~scroll_y_offset:b.scroll_y_offset ~text_wrap:b.text_wrap
                  in
                  if
                    List.exists
                      (fun char_code' -> char_code = char_code')
                      [ 1073741904; 1073741906; 1073741905; 1073741903 ]
                    || keysym = '\n' || keysym = '\r'
                  then
                    Ui.adjust_scrollbar_according_to_textarea_text_caret
                      ~box_containing_textarea:b
                      ~text_area_info:new_text_area_information;
                  b.content <- Some (Textarea new_text_area_information)
              | None -> ())
          | Sdl.MouseMotionEvt { x; y; _ }
            when Ui.is_within_box ~x ~y ~from_sdl_evt:true ~box:b -> (
              match b.bbox with
              | Some bbox ->
                  let ~font_info, .. =
                    Ui.TextTextureInfo.get_or_add_font_size_text_texture
                      ~font_size:
                        (Option.value b.font_size ~default:Freetype.font_size)
                  in
                  let new_info =
                    Ui_textarea.handle_mouse_motion_evt ~x ~y ~bbox ~font_info
                      ~rope:info.text ~text_area_information:info
                      ~text_wrap:b.text_wrap ~scroll_y_offset:b.scroll_y_offset
                  in
                  b.content <- Some (Textarea new_info)
              | None -> ())
          | Sdl.MouseButtonEvt { mouse_evt_type; x; y; _ }
            when Ui.is_within_box ~x ~y ~from_sdl_evt:true ~box:b -> (
              match b.bbox with
              | Some bbox -> (
                  let ~font_info, .. =
                    Ui.TextTextureInfo.get_or_add_font_size_text_texture
                      ~font_size:
                        (Option.value b.font_size ~default:Freetype.font_size)
                  in
                  match info.text with
                  | Some r -> (
                      let rope_pos =
                        Ui_textarea.find_closest_rope_pos_for_cursor_on_coords
                          ~text_wrap:b.text_wrap ~bbox ~font_info ~x ~y ~rope:r
                          ~scroll_y_offset:b.scroll_y_offset
                      in
                      match mouse_evt_type with
                      | Mousedown ->
                          b.content <-
                            Some
                              (Textarea
                                 {
                                   info with
                                   holding_mousedown_rope_pos = Some rope_pos;
                                   cursor_pos = Some rope_pos;
                                   highlight_pos = (Some rope_pos, None);
                                 })
                      | Mouseup
                        when info.holding_mousedown_rope_pos |> Option.is_some
                        ->
                          b.content <-
                            Some
                              (Textarea
                                 { info with holding_mousedown_rope_pos = None })
                      | _ -> ())
                  | None -> ())
              | None -> ())
          | Sdl.TextInputEvt { text; _ } ->
              let new_text_area_information =
                Ui_textarea.handle_txt_evt ~text_area_information:info ~text
              in
              Ui.adjust_scrollbar_according_to_textarea_text_caret
                ~box_containing_textarea:b
                ~text_area_info:new_text_area_information;
              b.content <- Some (Textarea new_text_area_information)
          | _ -> ())
      | None | _ -> ())
  | None -> ()

let check_for_holding ~(e : Sdl.event) =
  match e with
  | Sdl.KeyboardEvt { keysym; kbd_evt_type; _ } -> (
      if Char.code keysym = 1073742048 then
        match kbd_evt_type with
        | Keydown -> Ui.holding_ctrl := true
        | Keyup -> Ui.holding_ctrl := false)
  | Sdl.MouseButtonEvt { mouse_evt_type; x; y; _ } -> (
      match mouse_evt_type with
      | Mousedown -> Ui.holding_mousedown := `True (~original_x:x, ~original_y:y)
      | Mouseup -> Ui.holding_mousedown := `False)
  | _ -> ()

let emit_event ~(e : Sdl.event) =
  pass_evt_to_focused ~e;
  check_for_holding ~e;
  List.iter
    (fun (box, event_handler) -> event_handler ~b:box ~e)
    !event_handlers

let add_event_handler ~(box : Ui.box option)
    ~(event_handler : Ui.event_handler_t) =
  event_handlers := (box, event_handler) :: !event_handlers
