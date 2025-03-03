open Limitless.Sdl
open Limitless.Freetype

let () = init_sdl ()
let () = FreeType.freetype_init ()
let () = FreeType.freetype_load_font ()
let () = FreeType.freetype_set_pixel_sizes 6
let glyph_info = FreeType.freetype_load_glyph_letter 'f'

let glyph_infos =
  let startcode, endcode = (32, 126) in
  let rec get_glyph_info char_code acc =
    Printf.printf "%c" (Char.chr char_code);
    print_newline ();
    if char_code > endcode then acc
    else
      let new_glyph_info =
        FreeType.freetype_load_glyph_letter (Char.chr char_code)
      in
      get_glyph_info (succ char_code) ((char_code, new_glyph_info) :: acc)
  in
  get_glyph_info startcode []

let biggest_horiBearingY =
  List.fold_left
    (fun acc (_, g) -> max g.FreeType.metrics.horiBearingY acc)
    0 glyph_infos
;;

Printf.printf "%d" (List.length glyph_infos);
print_newline ()
;;

List.iter
  (fun (_, glyph) ->
    Printf.printf "%d\n" (Bytes.length glyph.FreeType.bitmap.buffer))
  glyph_infos

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
      sdl_set_render_draw_color w 0 255 0 200;
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
                ( PointF (Int.to_float x, Int.to_float y),
                  Bytes.get glyph_info.FreeType.bitmap.buffer
                    ((y * glyph_info.FreeType.bitmap.pitch) + x) ))
          pts
      in
      List.iter
        (function
          | PointF (x, y), byte ->
              let int_byte = Char.code byte in
              sdl_set_render_draw_color w 255 255 255 int_byte;
              sdl_render_draw_points_float w
                (* we are dividing by 3 here because of FT_RENDER_MODE_LCD *)
                [ PointF ((x /. 3.) +. fst offset, y +. snd offset) ])
        floats
  | None -> ()

let draw_glyphs () =
  let rec get_points x y acc (bitmap : FreeType.freetype_bitmap) =
    let new_acc = Point (x, y) :: acc in
    let width, rows = (bitmap.FreeType.width, bitmap.FreeType.rows) in
    if width > 0 && rows > 0 then
      match (x = width - 1, y = rows - 1) with
      | true, true -> new_acc
      | true, _ -> get_points 0 (succ y) new_acc bitmap
      | _ -> get_points (succ x) y new_acc bitmap
    else []
  in
  let rec get_xs glyphs x acc =
    match glyphs with
    | [] -> acc
    | (_, h) :: t -> get_xs t (x + fst h.FreeType.advance) (x :: acc)
  in
  let xs = get_xs glyph_infos 0 [] in
  let points_list =
    List.map
      (fun (_, glyph) -> (glyph, get_points 0 0 [] glyph.FreeType.bitmap))
      glyph_infos
  in
  List.iter2
    (fun (glyph, pl) x ->
      draw_bmp_points glyph pl
        ( Int.to_float (x + glyph.FreeType.metrics.horiBearingX),
          Int.to_float biggest_horiBearingY
          -. Int.to_float glyph.FreeType.metrics.horiBearingY ))
    points_list xs

let draw () =
  draw_rect ();
  draw_points points;
  draw_bmp_points glyph_info bmp_points (0., 0.);
  (*draw_glyphs ();*)
  match w with
  | Some w -> sdl_render_present w
  | None -> ()

let () = draw ()

let draw_letter_glyph letter (x, y) =
  let found_glyph =
    List.find_opt (fun (c, _) -> Char.chr c = letter) glyph_infos
  in
  match found_glyph with
  | Some (_, g) ->
      let rec get_points x y acc (bitmap : FreeType.freetype_bitmap) =
        let new_acc = Point (x, y) :: acc in
        let width, rows = (bitmap.FreeType.width, bitmap.FreeType.rows) in
        if width > 0 && rows > 0 then
          match (x = width - 1, y = rows - 1) with
          | true, true -> new_acc
          | true, _ -> get_points 0 (succ y) new_acc bitmap
          | _ -> get_points (succ x) y new_acc bitmap
        else []
      in
      let pts = get_points 0 0 [] g.FreeType.bitmap in
      (draw_bmp_points g pts
         ( Int.to_float x,
           Int.to_float y
           +. Int.to_float biggest_horiBearingY
           -. Int.to_float g.FreeType.metrics.horiBearingY );
       match w with Some w -> sdl_render_present w | None -> ());
      g.FreeType.advance
  | None -> (0, 0)

let rec loop (x, y) =
  let evt = sdl_pollevent () in
  let offset, continue =
    match evt with
    | Some (KeyboardEvt { keysym; timestamp; _ }) ->
        Printf.printf "KBD: %c, %d" keysym timestamp;
        print_newline ();
        let offset = draw_letter_glyph keysym (x, y) in
        (offset, true)
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
        ((0, 0), true)
    | Some (WindowEvt { event; _ }) -> (
        match event with
        | WindowClose -> ((0, 0), false)
        | WindowResize -> ((0, 0), true))
    | Some (MouseMotionEvt { x; y; _ }) ->
        Printf.printf "Mousemotion %d %d" x y;
        print_newline ();
        ((0, 0), true)
    | Some Quit -> ((0, 0), false)
    | None -> ((0, 0), true)
  in
  if continue then loop (x + fst offset, y + snd offset) else ()

let _ = loop (0, 0)
