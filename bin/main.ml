open Tack

let () = Sdl.sdl_gl_setswapinterval 0
let rendering_content = Editor.open_file "lib/ui_rendering.ml"

let textarea_with_line_numbers =
  Ui_textarea_with_line_numbers.create_textarea_with_line_numbers
    ~text:rendering_content ~textarea_width:1000 ~textarea_height:1000 ()

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
    content =
      Some
        (Boxes
           [
             file_explorer;
             {
               Ui.default_box with
               height_constraint = Some Max;
               width_constraint = Some Max;
               content = Some textarea_with_line_numbers;
             };
           ]);
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
