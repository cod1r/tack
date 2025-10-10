open Tack.Sdl
open Tack.Rope
open Tack.Ui_textarea
open Tack.Modes

let () = Sdl.sdl_gl_setswapinterval 0
let last_add = ref (Sdl.sdl_getticks ())

let rec loop () =
  let evt = Sdl.sdl_pollevent () in
  let continue =
    match evt with
    | Some Quit -> false
    | None ->
        (* Tack.Ui_rendering.draw ~box:Tack.Ui.box; *)
        true
    | Some e ->
        (* Tack.Ui_rendering.draw ~box:Tack.Ui.box; *)
        Tack.Ui_events.emit_event ~e;
        true
  in
  if continue then loop () else ()

let () = loop ()
