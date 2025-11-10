open Tack

let () = Sdl.sdl_gl_setswapinterval 0

let textarea : Ui.box =
  let textarea_box = Ui.create_textarea_box () in
  {
    textarea_box with
    name = Some "BOX";
    bbox = Some { x = 200; y = 200; width = 500; height = 500 };
    background_color = (0., 0.5, 0., 0.8);
    text_wrap = false;
    allow_horizontal_scroll = true;
    allow_vertical_scroll = true;
  }

let rec loop () =
  let evt = Sdl.sdl_pollevent () in
  let continue =
    match evt with
    | Some Quit -> false
    | None ->
        Ui_rendering.draw ~box:textarea;
        true
    | Some e ->
        Ui_events.emit_event ~e;
        Ui_rendering.draw ~box:textarea;
        true
  in
  if continue then loop () else ()

let () = loop ()
