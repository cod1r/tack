open Limitless.Events;;
open Limitless.Sdl;;
open Limitless.Freetype;;

let () = init_sdl ();;

let () = freetype_init ();;
let () = freetype_load_font ();;

let w = sdl_create_window "limitless" 0 0 800 800 (sdl_window_resizable);;

match w with
| Some(Window { width; height; title; _ }) ->
    Printf.printf "Created window: %s %d %d" title width height; print_newline ()
| None -> failwith "unable to create window";;

match w with
| Some(w) ->
    begin
      sdl_create_renderer w 1;
    end
| None -> ();;

let draw_rect () =
  match w with
  | Some(w) ->
    let rect = Rect { x=0; y=0; width=250; height=300 } in
    sdl_set_render_draw_color w 255 0 0 255;
    sdl_renderer_draw_rect w rect;
    sdl_renderer_fill_rect w rect;
  | None -> ();;

let draw_points points =
  match w with
  | Some(w) ->
      sdl_set_render_draw_color w 0 255 0 255;
      sdl_render_draw_points w points;
  | None -> ();;

let points =
  let rec points' x y acc =
    let new_point = Point (x, y) in
    match (x, y) with
    | (100, 100) -> new_point :: acc
    | (_, 100) -> points' (succ x) 0 (new_point :: acc)
    | _ -> points' x (succ y) (new_point :: acc)
  in
  points' 0 0 []
;;

let draw () =
  draw_rect ();
  draw_points points;
  match w with
  | Some(w) -> sdl_render_present w
  | None -> ();;

let () = draw ();;

let rec loop () =
  let evt = sdl_pollevent () in
  let continue = match evt with
    | Some(KeyboardEvt { keysym; timestamp; _ }) ->
        Printf.printf "KBD: %c, %d" keysym timestamp;
        print_newline ();
        true
    | Some(MouseButtonEvt {
        mouse_evt_type;
        timestamp;
        x;
        y;
        windowID;
        button;
        clicks;
      }) ->
        begin
          match mouse_evt_type with
          | Mousedown ->
              Printf.printf "Mousedown %d, %d, %d, %d, %d, %d" x y windowID button clicks timestamp; print_newline ();
          | Mouseup -> Printf.printf "Mouseup"; print_newline();
        end; true
    | Some(WindowEvt { event; _ }) ->
        begin
          match event with
          | WindowClose -> false
          | WindowResize -> true
        end
    | Some(MouseMotionEvt { x; y; _ }) ->
        begin
          Printf.printf "Mousemotion %d %d" x y; print_newline(); true
        end
    | None -> true in
    if continue then loop () else ();;

let _ = loop ();;
