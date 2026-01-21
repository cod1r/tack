open Tack

let () = Sdl.sdl_gl_setswapinterval 0

let box =
  { Ui.default_box with
    bbox = Some { x = 0; y = 0; width = 2000; height = 2000 }
  ; clip_content = true
  ; content =
      Some
        (Boxes
           (List.init 100 (fun _ ->
              { Ui.default_box with
                clip_content = true
              ; content =
                  Some
                    (Boxes
                       (List.init 100 (fun _ ->
                          { Ui.default_box with
                            clip_content = true
                          ; background_color =
                              Random.float 1., Random.float 1., Random.float 1., 1.0
                          ; font_size = Some 15
                          ; content = Some (Text { string = "20" })
                          ; bbox = Some { x = 0; y = 0; width = 20; height = 20 }
                          })))
              ; width_constraint = Some Min
              ; height_constraint = Some Min
              ; flow = Some Vertical
              })))
  ; flow = Some Horizontal
  }
;;

(* let box =
  { Ui.default_box with
    background_color= (0.8, 0.8, 0.8, 1.)
  ; bbox= Some {x= 10; y= 10; width= 200; height= 200}
  ; content= Some (Text {string= "HI THERE"})
  ; font_size= Some 37 }

let box =
  { Ui.default_box with
    clip_content= true
  ; background_color= (1., 0.5, 0.5, 1.)
  ; bbox= Some {x= 0; y= 0; width= 300; height= 200}
  ; content= Some (Box box) } *)

let rec loop () =
  let evt = Sdl.sdl_pollevent () in
  let continue =
    match evt with
    | Some Quit -> false
    | None ->
      Ui_rendering.draw ~box:Editor.editor_view;
      true
    | Some e ->
      Ui_events.emit_event ~e;
      Ui_rendering.draw ~box:Editor.editor_view;
      true
  in
  if continue then loop () else ()
;;

let () = loop ()
