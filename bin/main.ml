open Tack

let () = Sdl.sdl_gl_setswapinterval 0

(* let box =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 2000; height = 2000 }
  ; clip_content = true
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
                          ; bbox = Some { x = 0; y = 0; width = 20; height = 20 }
                          })))
              ; width_constraint = Some { constraint_type = Min; fallback_size = 0 }
              ; height_constraint = Some { constraint_type = Min; fallback_size = 0 }
              ; flow = Some Vertical
              })))
  ; flow = Some Horizontal
  }
;; *)

(* let box =
  { Ui.default_box with
    background_color= (0.8, 0.8, 0.8, 1.)
  ; bbox= Some {x= 10; y= 10; width= 200; height= 200}
  ; content= Some (Text {string= "HI THERE"})
  ; font_size= Some 37 }

let box =
  { Ui.default_box with
    clip_content= true
  ; background_color= (1., 0.5, 0.5, 1.)
  ; bbox= Some {x= 0; y= 0; width= 300; height= 200}
  ; content= Some (Box box) } *)

(* let window_box =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 0; height = 0 }
  ; content = Some (Box {
    Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 100; height = 100 }
    ; background_color = 0.3, 0., 0., 0.3
    ; content = Some (Text { string = "HEHEhdf;alskdjf;alsdkjf;alskdjf;asldkfja;sldkfja;dddddddd" })
    ; font_size = Some 26
  })
  ; background_color = 0., 0.3, 0., 0.5
  ; clip_content = true
  }
;;

let () =
  window_box.update
  <- Some
       (fun () ->
         let width_height = Sdl.sdl_gl_getdrawablesize () in
         let width, _ = width_height lsr 32, width_height land ((1 lsl 32) - 1) in
         (* Printf.printf "width: %d, height: %d\n" width height; flush_all (); *)
         window_box.bbox
         <- Some { x = 0; y = 0; width = width - 250; height = 35 })
;; *)

let window_box =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 0; height = 0 }
  ; content = Some (Box Editor.editor_view)
  ; background_color = 0., 0.3, 0., 0.5
  ; clip_content = true
  }
;;

let () =
  window_box.update
  <- Some
       (fun () ->
         let width_height = Sdl.sdl_gl_getdrawablesize () in
         let width, height = width_height lsr 32, width_height land ((1 lsl 32) - 1) in
         (* Printf.printf "width: %d, height: %d\n" width height; flush_all (); *)
         window_box.bbox
         <- Some { x = 0; y = 0; width = width - 200; height = height - 200 })
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
  (* Ui.print_box ~depth:5 Editor.editor_view
  |> fun b ->
  print_endline (Buffer.contents b); *)
  let continue, should_wait =
    match evt with
    | Some Quit -> false, false
    | None ->
      Ui_rendering.draw ~box:window_box;
      true, false
    | Some (WindowEvt { event; _ }) when event = WindowFocusLost -> true, true
    | Some e ->
      Ui_events.emit_event ~e;
      Ui_rendering.draw ~box:window_box;
      true, false
  in
  if continue then loop should_wait else ()
;;

let () = loop false
