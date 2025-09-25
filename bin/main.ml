open Tack.Sdl
open Tack.Editor
open Tack.Rope
open Tack.Render
open Tack.Modes

let () = Sdl.sdl_gl_setswapinterval 0

let rec loop () =
  let evt = Sdl.sdl_waitevent () in
  let (Tack.Ui.{ x; _ } as bbox_rest) =
    try Option.get Tack.Ui.text_box.bbox with Invalid_argument e -> failwith e
  in
  Tack.Ui.text_box.bbox <- Some { bbox_rest with x = x + 1 };
  let continue =
    match evt with
    | Some Quit -> false
    | None -> true
    | Some e ->
        Tack.Ui_rendering.draw ~box:Tack.Ui.box;
        Tack.Ui_events.emit_event ~e;
        true
  in
  if continue then loop () else ()

let () = loop ()
