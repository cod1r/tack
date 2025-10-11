open Tack.Sdl
open Tack.Rope
open Tack.Ui_textarea

let () = Sdl.sdl_gl_setswapinterval 0
let last_add = ref (Sdl.sdl_getticks ())

let text_box : Tack.Ui.box =
  {
    Tack.Ui.default_box with
    background_color = (0.5, 0.5, 0.5, 1.);
    bbox = Some { x = 0; y = 0; width = 100; height = 200 };
    content =
      Some
        (Tack.Ui.Textarea
           {
             text =
               Tack.Rope.of_string "hi there misses HAHAHAH"
               |> Tack.Rope.rebalance;
             cursor_pos = None;
             highlight_pos = None;
             scroll_y_offset = 0;
             scroll_x_offset = 0;
           });
  }

let files = Tack.Search.list_files "."

let file_tree =
  {
    Tack.Ui.default_box with
    content =
      Some
        (Boxes
           (List.map
              (fun s ->
                {
                  Tack.Ui.default_box with
                  content = Some (Text s);
                  bbox = Some { x = 0; y = 0; width = 50; height = 50 };
                })
              files));
    flow = Some Vertical;
  }

let box =
  {
    Tack.Ui.default_box with
    bbox = Some { x = 0; y = 0; height = 500; width = 500 };
    content = Some (Box file_tree);
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
