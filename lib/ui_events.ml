open Freetype

let event_handlers = ref []

let pass_evt_to_focused ~(e : Sdl.Sdl.event) =
  match !Ui.focused_element with
  | Some b -> (
      match b.content with
      | Some (Textarea info) -> (
          match e with
          | Sdl.Sdl.KeyboardEvt { kbd_evt_type; keysym; _ } ->
              let char_code = Char.code keysym in
              let font_info, _ =
                Ui_text_texture_info.get_or_add_font_size_text_texture
                  ~font_size:
                    (Option.value b.font_size ~default:FreeType.font_size)
              in
              let new_text_area_information =
                Ui_textarea.handle_kbd_evt
                  ~bbox:(Option.value b.bbox ~default:Ui.default_bbox)
                  ~font_info ~char_code ~keysym ~kbd_evt_type
                  ~text_area_information:info
              in
              b.content <- Some (Textarea new_text_area_information)
          | Sdl.Sdl.TextInputEvt { text; _ } ->
              let new_text_area_information =
                Ui_textarea.handle_txt_evt ~text_area_information:info ~text
              in
              b.content <- Some (Textarea new_text_area_information)
          | _ -> ())
      | None | _ -> ())
  | None -> ()

let emit_event ~(e : Sdl.Sdl.event) =
  pass_evt_to_focused ~e;
  List.iter
    (fun (box, event_handler) -> event_handler ~b:box ~e)
    !event_handlers

let add_event_handler ~(box : Ui.box option)
    ~(event_handler : Ui.event_handler_t) =
  event_handlers := (box, event_handler) :: !event_handlers
