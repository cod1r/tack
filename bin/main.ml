open Tack

let box =
  { Ui.default_box with
    clip_content = true
  ; content =
      Some
        (Boxes
           (List.init 100 (fun _ ->
              { Ui.default_box with
                clip_content = true
              ; content =
                  Some
                    (Boxes
                       (List.init 100 (fun _ ->
                          { Ui.default_box with
                            clip_content = true
                          ; background_color =
                              Random.float 1., Random.float 1., Random.float 1., 1.0
                          ; font_size = Some 15
                          ; content = Some (Text { string = "20" })
                          ; width_constraint = Some (Number 30)
                          ; height_constraint = Some (Content { fallback_size = 0 })
                          })))
              ; width_constraint = Some (Content { fallback_size = 0 })
              ; height_constraint = Some (Content { fallback_size = 0 })
              ; flow = Some Vertical
              })))
  ; flow = Some Horizontal
  ; width_constraint = Some (Number 1000)
  ; height_constraint = Some (Number 1000)
  ; background_color = 0.5, 0.1, 0., 0.5
  }
;;

let window_box =
  { Ui.default_box with
    content = Some (Box Editor.editor_view)
  ; background_color = 0.5, 0.3, 0., 0.5
  ; clip_content = true
  ; width_constraint = Some (Number 1800)
  ; height_constraint = Some (Number 1600)
  }
;;

let () =
  Ui_events.add_event_handler ~box:None ~event_handler:(fun ~b ~e ->
    let _ = b in
    match e with
    | Sdl.MouseButtonEvt { x; y; mouse_evt_type; _ } ->
      if mouse_evt_type = Mousedown
      then (
        Printf.printf "%d %d\n" (x * 2) (y * 2);
        flush_all ())
    | _ -> ())
;;

let rec loop should_wait =
  let evt = Sdl.sdl_pollevent should_wait in
  let continue, should_wait =
    match evt with
    | Some Quit -> false, false
    | None ->
      Ui_rendering.draw ~box;
      true, false
    | Some (WindowEvt { event; _ }) when event = WindowFocusLost -> true, true
    | Some e ->
      Ui_events.emit_event ~e;
      Ui_rendering.draw ~box;
      true, false
  in
  if continue then loop should_wait else ()
;;

let () = loop false
