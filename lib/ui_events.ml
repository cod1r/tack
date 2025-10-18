open Freetype
open Sdl

let event_handlers = ref []

let pass_evt_to_focused ~(e : Sdl.event) =
  match !Ui.focused_element with
  | Some b -> (
      match b.content with
      | Some (Textarea info) -> (
          match e with
          | Sdl.KeyboardEvt { kbd_evt_type; keysym; _ } ->
              let char_code = Char.code keysym in
              let font_info, _ =
                Ui_text_texture_info.get_or_add_font_size_text_texture
                  ~font_size:
                    (Option.value b.font_size ~default:Freetype.font_size)
              in
              let new_text_area_information =
                Ui_textarea.handle_kbd_evt
                  ~bbox:(Option.value b.bbox ~default:Ui.default_bbox)
                  ~font_info ~char_code ~keysym ~kbd_evt_type
                  ~text_area_information:info
              in
              b.content <- Some (Textarea new_text_area_information)
          | Sdl.MouseButtonEvt { mouse_evt_type; x; y; _ } -> (
              match b.bbox with
              | Some bbox -> (
                  let font_info, _ =
                    Ui_text_texture_info.get_or_add_font_size_text_texture
                      ~font_size:
                        (Option.value b.font_size ~default:Freetype.font_size)
                  in
                  match info.text with
                  | Some r -> (
                      let rope_pos =
                        Ui_textarea.find_closest_rope_pos_for_cursor_on_coords
                          ~bbox ~font_info ~x ~y ~rope:r
                          ~scroll_y_offset:info.scroll_y_offset
                      in
                      match mouse_evt_type with
                      | Mousedown ->
                          b.content <-
                            Some
                              (Textarea
                                 {
                                   info with
                                   holding_mousedown = true;
                                   cursor_pos = Some rope_pos;
                                   highlight_pos = (Some rope_pos, None);
                                 })
                      | Mouseup when info.holding_mousedown = true ->
                          let start = fst info.highlight_pos in
                          b.content <-
                            Some
                              (Textarea
                                 {
                                   info with
                                   holding_mousedown = false;
                                   cursor_pos = Some rope_pos;
                                   highlight_pos = (start, Some rope_pos);
                                 })
                      | _ -> ())
                  | None -> ())
              | None -> ())
          | Sdl.TextInputEvt { text; _ } ->
              let new_text_area_information =
                Ui_textarea.handle_txt_evt ~text_area_information:info ~text
              in
              b.content <- Some (Textarea new_text_area_information)
          | _ -> ())
      | None | _ -> ())
  | None -> ()

let emit_event ~(e : Sdl.event) =
  pass_evt_to_focused ~e;
  List.iter
    (fun (box, event_handler) -> event_handler ~b:box ~e)
    !event_handlers

let add_event_handler ~(box : Ui.box option)
    ~(event_handler : Ui.event_handler_t) =
  event_handlers := (box, event_handler) :: !event_handlers
