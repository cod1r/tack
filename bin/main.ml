open Tack

let () = Sdl.sdl_gl_setswapinterval 0
let rendering_content = Editor.open_file "lib/ui_rendering.ml"

let textarea : Ui.box =
  let textarea_box = Ui.create_textarea_box () in
  {
    textarea_box with
    focusable = true;
    bbox = Some { x = 0; y = 0; width = 1700; height = 0 };
    background_color = (0., 0.5, 0., 0.8);
    text_wrap = true;
    allow_horizontal_scroll = true;
    allow_vertical_scroll = true;
    height_constraint = Some Max;
    content =
      Some
        (Textarea
           {
             Ui.default_text_area_information with
             text = Some rendering_content;
           });
  }

let file_explorer =
  {
    Ui.default_box with
    bbox = Some { x = 0; y = 0; height = 0; width = 300 };
    height_constraint = Some Max;
    background_color = (0.0, 0.0, 0.3, 1.);
  }

let box =
  {
    Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 2000; height = 2000 };
    content = Some (Boxes [ file_explorer; textarea ]);
    flow = Some Horizontal;
  }

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

let () = loop ()
