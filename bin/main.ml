open Tack

let () = Sdl.sdl_gl_setswapinterval 0
let last_add = ref (Sdl.sdl_getticks ())

let scroll_bar =
  { Ui.default_box with
    height_constraint = Some Max
  ; bbox =
      Some
        { x = 0; y = 0; width = 10; height = 0; inner_width = None; inner_height = None }
  ; background_color = 0., 0., 0., 1.
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
        ; height = 100
        ; inner_width = None
        ; inner_height = None
        }
  ; content = Some (Box scroll_bar)
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
