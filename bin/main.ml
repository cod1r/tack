open Tack.Sdl
open Tack.Rope
open Tack.Ui_textarea
open Tack.Modes

let () = Sdl.sdl_gl_setswapinterval 0
let last_add = ref (Sdl.sdl_getticks ())

let box: Tack.Ui.box = {
  Tack.Ui.default_box with
  bbox = Some { x = 0; y = 0; width = 100; height = 200 };
  content = Some (Tack.Ui.Textarea {
    text = Tack.Rope.of_string "HI THERE MISSES HAHAHAH" |> Tack.Rope.rebalance;
    cursor_pos = None;
    highlight_pos = None;
    scroll_y_offset = 0;
    scroll_x_offset = 0;
  })
}

let rec loop () =
  let evt = Sdl.sdl_pollevent () in
  let continue =
    match evt with
    | Some Quit -> false
    | None ->
        Tack.Ui_rendering.draw ~box;
        true
    | Some e ->
        Tack.Ui_rendering.draw ~box;
        Tack.Ui_events.emit_event ~e;
        true
  in
  if continue then loop () else ()

let () = loop ()
