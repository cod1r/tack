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
          | Sdl.MouseWheelEvt { x; y; mouseX; mouseY }
            when Ui.is_within_box ~x:mouseX ~y:mouseY ~box:b ~from_sdl_evt:true
            -> (
              match
                List.find_opt
                  (function
                    | Ui.ScrollContainer { content; _ } -> content == b
                    | _ -> failwith "impossible")
                  !Ui.scrollcontainers
              with
              | Some
                  (ScrollContainer
                     {
                       scroll;
                       orientation;
                       other_scrollcontainer;
                       scrollbar_container;
                       _;
                     }) -> (
                  let adjust_scroll ~scroll = function
                    | Ui.Vertical ->
                        Option.iter
                          (fun bbox ->
                            let Ui.
                                  {
                                    y = scrollbar_container_y;
                                    height = scrollbar_container_height;
                                    _;
                                  } =
                              Option.get scrollbar_container.bbox
                            in
                            scroll.Ui.bbox <-
                              Some
                                {
                                  bbox with
                                  y =
                                    min
                                      (scrollbar_container_y
                                     + scrollbar_container_height
                                     - bbox.Ui.height)
                                      (max scrollbar_container_y (bbox.Ui.y + -y));
                                })
                          scroll.bbox
                    | Horizontal ->
                        Option.iter
                          (fun bbox ->
                            let Ui.
                                  {
                                    x = scrollbar_container_x;
                                    width = scrollbar_container_width;
                                    _;
                                  } =
                              Option.get scrollbar_container.bbox
                            in
                            scroll.bbox <-
                              Some
                                {
                                  bbox with
                                  x =
                                    min
                                      (scrollbar_container_x
                                     + scrollbar_container_width - bbox.Ui.width
                                      )
                                      (max scrollbar_container_x (bbox.Ui.x + -x));
                                })
                          scroll.bbox
                  in
                  adjust_scroll ~scroll orientation;
                  match other_scrollcontainer with
                  | Some { scroll; orientation; _ } ->
                      adjust_scroll ~scroll orientation
                  | _ -> ())
              | _ -> ())
          | Sdl.TextInputEvt { text; _ } ->
              let new_text_area_information =
                Ui_textarea.handle_txt_evt ~text_area_information:info ~text
              in
              (match
                 List.find_opt
                   (function
                     | Ui.ScrollContainer { content; _ } -> content == b
                     | _ -> failwith "impossible")
                   !Ui.scrollcontainers
               with
              | Some
                  (ScrollContainer
                     { scroll; orientation; other_scrollcontainer; content; _ })
                -> (
                  Ui.adjust_scrollbar_according_to_textarea_text_caret
                    ~text_area_info:new_text_area_information ~scroll
                    ~orientation ~content;
                  match other_scrollcontainer with
                  | Some { scroll; orientation; content; _ } ->
                      Ui.adjust_scrollbar_according_to_textarea_text_caret
                        ~text_area_info:new_text_area_information ~scroll
                        ~orientation ~content
                  | _ -> ())
              | _ -> ());
              b.content <- Some (Textarea new_text_area_information)
          | _ -> ())
      | _ -> ())
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

(* the reason that it's a box option is because sometimes there are going to be global event handlers *)
let add_event_handler ~(box : Ui.box option)
    ~(event_handler : Ui.event_handler_t) =
  if
    not
      (List.exists
         (fun (_, evt_handler) -> evt_handler == event_handler)
         !event_handlers)
  then event_handlers := (box, event_handler) :: !event_handlers

let remove_event_handler ~(box : Ui.box) =
  event_handlers :=
    List.filter
      (fun (box', _) -> Option.is_none box' || Option.get box' != box)
      !event_handlers
