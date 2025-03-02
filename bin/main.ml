open Limitless.Sdl
open Limitless.Freetype

let () = init_sdl ()
let () = freetype_init ()
let () = freetype_load_font ()
let () = freetype_set_pixel_sizes 8
let glyph_info = freetype_load_glyph_letter 'f'

let glyph_infos =
  let startcode, endcode = (32, 126) in
  let rec get_glyph_info char_code acc =
    Printf.printf "%c" (Char.chr char_code); print_newline ();
    if char_code > endcode then
      acc
    else
      let new_glyph_info = freetype_load_glyph_letter (Char.chr char_code) in
      get_glyph_info (succ char_code) (new_glyph_info :: acc)
  in
  get_glyph_info startcode []
;;

Printf.printf "%d" (List.length glyph_infos); print_newline ();;
List.iter (fun glyph -> Printf.printf "%d\n" (Bytes.length glyph.bitmap.buffer)) glyph_infos

let w =
  sdl_create_window "limitless" 0 0 800 800
    (sdl_window_resizable lor sdl_window_allow_highdpi)
;;

match w with
| Some (Window { width; height; title; _ }) ->
    Printf.printf "Created window: %s %d %d" title width height;
    print_newline ()
| None -> failwith "unable to create window"
;;

match w with
| Some w ->
    sdl_create_renderer w sdl_renderer_software;
    sdl_set_render_draw_blendmode w sdl_blendmode_blend
| None -> ()

let draw_rect () =
  match w with
  | Some w ->
      let rect = Rect { x = 0; y = 0; width = 250; height = 300 } in
      sdl_set_render_draw_color w 255 0 0 255;
      sdl_renderer_draw_rect w rect;
      sdl_renderer_fill_rect w rect
  | None -> ()

let draw_points points =
  match w with
  | Some w ->
      sdl_set_render_draw_color w 0 255 0 100;
      sdl_render_draw_points w points
  | None -> ()

let points =
  let rec points' x y acc =
    let new_point = Point (x, y) in
    match (x, y) with
    | 100, 100 -> new_point :: acc
    | _, 100 -> points' (succ x) 0 (new_point :: acc)
    | _ -> points' x (succ y) (new_point :: acc)
  in
  points' 0 0 []

let bmp_points =
  let rec get_points x y acc =
    let new_acc = Point (x, y) :: acc in
    match (x = glyph_info.bitmap.width - 1, y = glyph_info.bitmap.rows - 1) with
    | true, true -> new_acc
    | true, _ -> get_points 0 (succ y) new_acc
    | _ -> get_points (succ x) y new_acc
  in
  get_points 0 0 []

let draw_bmp_points glyph_info pts offset =
  match w with
  | Some w ->
      let floats =
        List.map
          (function
            | Point (x, y) ->
                (*Printf.printf "what %d %d" (Bytes.length glyph_info.bitmap.buffer) ((y * glyph_info.bitmap.pitch) + x); print_newline ();*)
                ( PointF (Int.to_float x, Int.to_float y),
                  Bytes.get glyph_info.bitmap.buffer
                    ((y * glyph_info.bitmap.pitch) + x) ))
          pts
      in
      List.iter
        (function
          | PointF (x, y), byte ->
              let int_byte = Char.code byte in
              sdl_set_render_draw_color w 255 255 255 int_byte;
              sdl_render_draw_points_float w [ PointF (x /. 3. +. fst offset, y +. snd offset) ])
        floats
  | None -> ()

let draw_glyphs () =
  let rec get_points x y acc (bitmap: freetype_bitmap) =
    let new_acc = Point (x, y) :: acc in
    let (width, rows) = (bitmap.width, bitmap.rows) in
    if width > 0 && rows > 0 then
      match (x = width - 1, y = rows - 1) with
      | true, true -> new_acc
      | true, _ -> get_points 0 (succ y) new_acc bitmap
      | _ -> get_points (succ x) y new_acc bitmap
    else
      []
  in
  let rec get_xs glyphs x acc =
    match glyphs with
    | [] -> acc
    | h :: t -> get_xs t (x + fst h.advance) (x :: acc)
  in
  let xs = get_xs glyph_infos 0 [] in
  let points_list = List.map (fun glyph -> (glyph, get_points 0 0 [] glyph.bitmap)) glyph_infos in
  (*List.iter (function | Point (x, y) -> Printf.printf "%d %d" x y; print_newline ()) (List.flatten points_list)*)
  List.iter2 (fun (glyph, pl) x -> draw_bmp_points glyph pl (Int.to_float x, 0.)) points_list xs
;;

let draw () =
  draw_rect ();
  draw_points points;
  draw_bmp_points glyph_info bmp_points (0., 0.);
  draw_glyphs ();
  match w with Some w -> sdl_render_present w | None -> ()

let () = draw ()

let rec loop () =
  let evt = sdl_pollevent () in
  let continue =
    match evt with
    | Some (KeyboardEvt { keysym; timestamp; _ }) ->
        Printf.printf "KBD: %c, %d" keysym timestamp;
        print_newline ();
        true
    | Some
        (MouseButtonEvt
           { mouse_evt_type; timestamp; x; y; windowID; button; clicks }) ->
        (match mouse_evt_type with
        | Mousedown ->
            Printf.printf "Mousedown %d, %d, %d, %d, %d, %d" x y windowID button
              clicks timestamp;
            print_newline ()
        | Mouseup ->
            Printf.printf "Mouseup";
            print_newline ());
        true
    | Some (WindowEvt { event; _ }) -> (
        match event with WindowClose -> false | WindowResize -> true)
    | Some (MouseMotionEvt { x; y; _ }) ->
        Printf.printf "Mousemotion %d %d" x y;
        print_newline ();
        true
    | Some Quit -> false
    | None -> true
  in
  if continue then loop () else ()

let _ = loop ()
