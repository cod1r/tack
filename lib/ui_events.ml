open Ui_types

let event_handlers = ref []

let adjust_scroll_container_for_focused_element ?mouse_pos_xy b new_text_area_information =
  match List.find_opt (fun (b', _) -> b' == b) !Ui_globals.scrollcontainers with
  | Some (_, scrollcontainer_info) ->
    Ui.adjust_scrollbar_according_to_textarea_text_caret
      ~mouse_pos_xy
      ~text_area_info:new_text_area_information
      ~scrollcontainer_info
      ~content:b
  | _ -> ()
;;

let pass_evt_to_focused ~(e : Sdl.event) =
  match !Ui_globals.focused_element with
  | Some b ->
    (match b.content with
     | Some (Textarea info) ->
       (match e with
        | Sdl.KeyboardEvt { kbd_evt_type; keysym; _ } ->
          let char_code = Char.code keysym in
          let ~font_info, .. =
            Ui.TextTextureInfo.get_or_add_font_size_text_texture
              ~font_size:(Option.value b.font_size ~default:Freetype.font_size)
          in
          (match b.bbox with
           | Some _ ->
             let new_text_area_information =
               Ui_textarea.handle_kbd_evt
                 ~box:b
                 ~font_info
                 ~char_code
                 ~keysym
                 ~kbd_evt_type
                 ~text_area_information:info
             in
             adjust_scroll_container_for_focused_element b new_text_area_information;
             b.content <- Some (Textarea new_text_area_information)
           | None -> ())
        | Sdl.MouseMotionEvt { x; y; _ }
          when match !Ui_globals.holding_mousedown with
               | `True (~original_x, ~original_y) ->
                 Ui.is_within_box ~x:original_x ~y:original_y ~from_sdl_evt:true ~box:b
               | _ -> false ->
          (match b.bbox with
           | Some _ ->
             let ~font_info, .. =
               Ui.TextTextureInfo.get_or_add_font_size_text_texture
                 ~font_size:(Option.value b.font_size ~default:Freetype.font_size)
             in
             let new_info =
               Ui_textarea.handle_mouse_motion_evt
                 ~x
                 ~y
                 ~box:b
                 ~font_info
                 ~rope:info.text
                 ~text_area_information:info
             in
             let width_ratio, height_ratio =
               Sdl.get_logical_to_opengl_window_dims_ratio ()
             in
             let x, y = x * width_ratio, y * height_ratio in
             adjust_scroll_container_for_focused_element ~mouse_pos_xy:(x, y) b new_info;
             b.content <- Some (Textarea new_info)
           | None -> ())
        | Sdl.MouseButtonEvt { mouse_evt_type; x; y; _ }
          when (Ui.is_within_box ~x ~y ~from_sdl_evt:true ~box:b
                && mouse_evt_type = Mousedown)
               || mouse_evt_type = Mouseup ->
          (match b.bbox with
           | Some _ ->
             let ~font_info, .. =
               Ui.TextTextureInfo.get_or_add_font_size_text_texture
                 ~font_size:(Option.value b.font_size ~default:Freetype.font_size)
             in
             (match info.text with
              | Some r ->
                let rope_pos =
                  Ui_textarea.find_closest_rope_pos_for_cursor_on_coords
                    ~box:b
                    ~font_info
                    ~x
                    ~y
                    ~rope:r
                in
                (match mouse_evt_type with
                 | Mousedown ->
                   b.content
                   <- Some
                        (Textarea
                           { info with
                             holding_mousedown_rope_pos = Some rope_pos
                           ; cursor_pos = Some rope_pos
                           ; highlight_pos = Some rope_pos, None
                           })
                 | Mouseup when info.holding_mousedown_rope_pos |> Option.is_some ->
                   b.content
                   <- Some (Textarea { info with holding_mousedown_rope_pos = None })
                 | _ -> ())
              | None -> ())
           | None -> ())
        | Sdl.MouseWheelEvt { x; y; mouseX; mouseY }
          when Ui.is_within_box ~x:mouseX ~y:mouseY ~box:b ~from_sdl_evt:true ->
          (match List.find_opt (fun (b', _) -> b' == b) !Ui_globals.scrollcontainers with
           | Some (_, { vertical_scroll_info; horizontal_scroll_info }) ->
             let width_ratio, height_ratio =
               Sdl.get_logical_to_opengl_window_dims_ratio ()
             in
             let y = y * height_ratio
             and x = x * width_ratio in
             let adjust_scroll ~scroll ~scrollbar_container = function
               | Vertical ->
                 assert (scroll.bbox <> None);
                 assert (scrollbar_container.bbox <> None);
                 let bbox = Option.get scroll.bbox in
                 let { y = scrollbar_container_y; height = scrollbar_container_height; _ }
                   =
                   Option.get scrollbar_container.bbox
                 in
                 scroll.bbox
                 <- Some
                      { bbox with
                        y =
                          min
                            (scrollbar_container_y
                             + scrollbar_container_height
                             - bbox.height)
                            (max scrollbar_container_y (bbox.y + -y))
                      }
               | Horizontal ->
                 assert (scroll.bbox <> None);
                 assert (scrollbar_container.bbox <> None);
                 let bbox = Option.get scroll.bbox in
                 let { x = scrollbar_container_x; width = scrollbar_container_width; _ } =
                   Option.get scrollbar_container.bbox
                 in
                 scroll.bbox
                 <- Some
                      { bbox with
                        x =
                          min
                            (scrollbar_container_x
                             + scrollbar_container_width
                             - bbox.width)
                            (max scrollbar_container_x (bbox.x + x))
                      }
             in
             (match vertical_scroll_info with
              | Some
                  { vertical_scroll = scroll
                  ; vertical_scrollbar_container = scrollbar_container
                  } -> adjust_scroll ~scroll ~scrollbar_container Vertical
              | None -> ());
             (match horizontal_scroll_info with
              | Some
                  { horizontal_scroll = scroll
                  ; horizontal_scrollbar_container = scrollbar_container
                  } -> adjust_scroll ~scroll ~scrollbar_container Horizontal
              | None -> ())
           | _ -> ())
        | Sdl.TextInputEvt { text; _ } ->
          let new_text_area_information =
            Ui_textarea.handle_txt_evt ~text_area_information:info ~text
          in
          b.content <- Some (Textarea new_text_area_information)
        | _ -> ())
     | _ -> ())
  | None -> ()
;;

let check_for_holding ~(e : Sdl.event) =
  match e with
  | KeyboardEvt { keysym; kbd_evt_type; _ } ->
    if Char.code keysym = 1073742048
    then (
      match kbd_evt_type with
      | Keydown -> Ui_globals.holding_ctrl := true
      | Keyup -> Ui_globals.holding_ctrl := false)
  | MouseButtonEvt { mouse_evt_type; x; y; _ } ->
    (match mouse_evt_type with
     | Mousedown -> Ui_globals.holding_mousedown := `True (~original_x:x, ~original_y:y)
     | Mouseup -> Ui_globals.holding_mousedown := `False)
  | _ -> ()
;;

let emit_event ~(e : Sdl.event) =
  pass_evt_to_focused ~e;
  check_for_holding ~e;
  List.iter (fun (box, event_handler) -> event_handler ~b:box ~e) !event_handlers
;;

(* the reason that it's a box option is because sometimes there are going to be global event handlers *)
let add_event_handler ~(box : box option) ~(event_handler : event_handler_t) =
  if
    not
      (List.exists
         (fun (b, evt_handler) ->
            evt_handler == event_handler
            &&
            match box, b with
            | Some b, Some b' -> b == b'
            | _ -> false)
         !event_handlers)
  then event_handlers := (box, event_handler) :: !event_handlers
;;

let remove_event_handler ~(box : box) =
  event_handlers
  := List.filter
       (fun (box', _) -> Option.is_none box' || Option.get box' != box)
       !event_handlers
;;
