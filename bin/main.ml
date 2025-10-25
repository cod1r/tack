open Tack

let () = Sdl.sdl_gl_setswapinterval 0
let last_add = ref (Sdl.sdl_getticks ())

let scroll_bar =
  { Ui.default_box with
    height_constraint = Some Max
  ; bbox =
      Some
        { x = 0; y = 0; width = 15; height = 0; inner_width = None; inner_height = None }
  ; background_color = 0.2, 0.2, 0.2, 0.5
  ; content =
      Some
        (Box
           { Ui.default_box with
             bbox =
               Some
                 { x = 0
                 ; y = 0
                 ; width = 8
                 ; height = 50
                 ; inner_width = None
                 ; inner_height = None
                 }
           ; background_color = 0., 0., 0., 1.
           })
  ; horizontal_align = Some Center
  }
;;

let box =
  { Ui.default_box with
    horizontal_align = Some Right
  ; bbox =
      Some
        { x = 0
        ; y = 0
        ; width = 100
        ; height = 200
        ; inner_width = None
        ; inner_height = None
        }
  ; content = Some (Box scroll_bar)
  ; background_color = 0.5, 0.5, 0.5, 1.
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
