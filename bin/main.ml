open Tack

let () = Sdl.sdl_gl_setswapinterval 0

let textarea : Ui.box =
  let textarea_box = Ui.create_textarea_box () in
  {
    textarea_box with
    bbox = Some { x = 200; y = 200; width = 500; height = 500 };
    background_color = (0., 0.5, 0., 0.8);
    text_wrap = false;
  }

let scroll_container =
  Ui.create_scrollcontainer ~content:textarea ~orientation:Horizontal
    ~other_scrollcontainer:None

let scroll_container =
  Ui.create_scrollcontainer ~content:textarea ~orientation:Vertical
    ~other_scrollcontainer:
      (Some
         (match scroll_container with
         | ScrollContainer info -> info
         | _ -> failwith "impossible"))

let box =
  {
    Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 500; height = 500 };
    content = Some scroll_container;
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
