open Limitless.Sdl;;
open Limitless.Freetype;;

let () = init_sdl ();;

let () = freetype_init ();;
let () = freetype_load_font ();;
let () = freetype_set_pixel_sizes 8;;

let bmp = freetype_load_glyph_letter 'a';;

let w = sdl_create_window "limitless" 0 0 800 800 (sdl_window_resizable lor sdl_window_allow_highdpi);;

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

let bmp_points =
  let rec get_points x y acc =
    let byte = Bytes.get (bmp.buffer) (y * bmp.pitch + x) in
    let new_acc = if byte = Char.chr 0 then acc else (Point (x, y) :: acc) in
    match (x = bmp.width - 1, y = bmp.rows - 1) with
    | (true, true) -> new_acc
    | (true, _) -> get_points 0 (succ y) new_acc
    | _ -> get_points (succ x) y new_acc
  in
  get_points 0 0 []
;;

let draw_bmp_points pts =
  match w with
  | Some(w) ->
    let floats = List.map (function | Point(x, y) -> (PointF (Int.to_float x, Int.to_float y), Bytes.get bmp.buffer (y * bmp.pitch + x))) pts in
    List.iter (function | (PointF (x, y), byte) ->
      let int_byte = Char.code byte in
      sdl_set_render_draw_color w 0 0 0 int_byte;
      sdl_render_draw_points_float w [PointF (x /. 3., y)]) floats
  | None -> ()
;;

let draw () =
  draw_rect ();
  draw_points points;
  draw_bmp_points bmp_points;
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
    | Some(Quit) -> false
    | None -> true in
    if continue then loop () else ();;

let _ = loop ();;
