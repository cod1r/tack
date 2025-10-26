open Tack

let () = Sdl.sdl_gl_setswapinterval 0
let last_add = ref (Sdl.sdl_getticks ())

let scroll_bar =
  { Ui.default_box with
    height_constraint = Some Max
  ; bbox = Some { x = 0; y = 0; width = 15; height = 0 }
  ; background_color = 0., 1., 1., 1.
  ; content =
      Some
        (Box
           { Ui.default_box with
             bbox = Some { x = 0; y = 0; width = 8; height = 50 }
           ; background_color = 0., 0., 0., 1.
           })
  ; horizontal_align = Some Center
  }
;;

let textarea : Ui.box =
  { Ui.default_box with
    content = Some (Textarea Ui.default_text_area_information)
  ; width_constraint = Some Max
  ; height_constraint = Some Max
  ; background_color = 0., 0.5, 0., 0.8
  }
;;

let box =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 100; height = 200 }
  ; content = Some (Boxes [ textarea; scroll_bar ])
  ; flow = Some Horizontal
  }
;;

let rec loop () =
  let evt = Sdl.sdl_pollevent () in
  let continue =
    match evt with
    | Some Quit -> false
    | None ->
      Ui_rendering.draw ~box;
      true
    | Some e ->
      Ui_events.emit_event ~e;
      Ui_rendering.draw ~box;
      true
  in
  if continue then loop () else ()
;;

let () = loop ()
