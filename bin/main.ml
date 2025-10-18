open Tack

let () = Sdl.sdl_gl_setswapinterval 0
let last_add = ref (Sdl.sdl_getticks ())

let box'' =
  {
    Ui.default_box with
    name = Some "SECOND";
    bbox = Some { x = 0; y = 0; height = 500; width = 500 };
    background_color = (0.5, 0.5, 0.5, 1.0);
    on_event =
      Some
        (fun ~b ~e ->
          match e with
          | Sdl.MouseButtonEvt { mouse_evt_type; x; y; _ } ->
              if mouse_evt_type = Mousedown then
                let Ui.{ top; left; bottom; right } =
                  Ui.get_box_sides ~box:(Option.get b)
                in
                let x, y = (x * 2, y * 2) in
                if x >= left && x <= right && y >= top && y <= bottom then
                  Ui.set_focused_element ~box:(Option.get b)
          | _ -> ());
    content =
      Some
        (Textarea
           {
             text = Some (Rope.of_string "HI" |> Rope.rebalance);
             cursor_pos = None;
             highlight_pos = (None, None);
             holding_mousedown = false;
             holding_ctrl = false;
             scroll_y_offset = 0;
             scroll_x_offset = 0;
           });
  }

let box' =
  {
    Ui.default_box with
    name = Some "FIRST";
    bbox = Some { x = 0; y = 0; height = 300; width = 300 };
    background_color = (0.8, 0.8, 0.8, 1.0);
    font_size = Some 14;
    on_event =
      Some
        (fun ~b ~e ->
          match e with
          | Sdl.MouseButtonEvt { mouse_evt_type; x; y; _ } -> (
              if mouse_evt_type = Mousedown then
                match b with
                | Some b -> (
                    match b.bbox with
                    | Some _ ->
                        let Ui.{ top; left; bottom; right } =
                          Ui.get_box_sides ~box:b
                        in
                        let x, y = (x * 2, y * 2) in
                        if x >= left && x <= right && y >= top && y <= bottom
                        then (
                          Ui.set_focused_element ~box:b;
                          b.background_color <-
                            ( Random.float 1.,
                              Random.float 1.,
                              Random.float 1.,
                              1. ))
                    | None -> ())
                | None -> ())
          | _ -> ());
    content =
      Some
        (Textarea
           {
             text = Some (Rope.of_string "HI" |> Rope.rebalance);
             cursor_pos = None;
             highlight_pos = (None, None);
             holding_mousedown = false;
             holding_ctrl = false;
             scroll_y_offset = 0;
             scroll_x_offset = 0;
           });
  }

let smol_box =
  {
    Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 200; height = 200 };
    on_event =
      Some
        (fun ~b ~e ->
          match e with
          | Sdl.MouseMotionEvt { x; y; _ } ->
              let box = Option.get b in
              let x, y = (x * 2, y * 2) in
              let Ui.{ top; bottom; left; right } =
                Ui.get_box_sides ~box
              in
              if x >= left && x <= right && y >= top && y <= bottom then
                box.background_color <-
                  (Random.float 1., Random.float 1., Random.float 1., 1.)
          | _ -> ());
  }

let box =
  {
    Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 1000; height = 1000 };
    content = Some (Boxes [ box'; box''; smol_box ]);
    flow = Some Horizontal;
  }

let rec loop () =
  let evt = Sdl.sdl_pollevent () in
  let continue =
    match evt with
    | Some Quit -> false
    | None ->
        Ui_rendering.draw ~box:box';
        true
    | Some e ->
        Ui_events.emit_event ~e;
        Ui_rendering.draw ~box:box';
        true
  in
  if continue then loop () else ()

let () = loop ()
