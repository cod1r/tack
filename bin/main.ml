open Tack.Sdl
open Tack.Rope
open Tack.Ui_textarea

let () = Sdl.sdl_gl_setswapinterval 0
let last_add = ref (Sdl.sdl_getticks ())

let box =
  {
    Tack.Ui.default_box with
    bbox = Some { x = 0; y = 0; height = 1000; width = 1000 };
    background_color = (0.8, 0.8, 0.8, 1.0);
    on_event =
      Some
        (fun ~b ~e ->
          match e with
          | Sdl.MouseButtonEvt { mouse_evt_type; _ } -> (
              if mouse_evt_type = Mousedown then
                match b with
                | Some b -> Tack.Ui.set_focused_element ~box:b
                | None -> ())
          | _ -> ());
    content =
      Some
        (Textarea
           {
             text = Some (Tack.Rope.of_string "HI" |> Tack.Rope.rebalance);
             cursor_pos = None;
             highlight_pos = None;
             holding_mousedown = false;
             holding_ctrl = false;
             scroll_y_offset = 0;
             scroll_x_offset = 0;
           });
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
        Tack.Ui_events.emit_event ~e;
        Tack.Ui_rendering.draw ~box;
        true
  in
  if continue then loop () else ()

let () = loop ()
