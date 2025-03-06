open Limitless.Sdl
open Limitless.Freetype

let () = init_sdl ()
let () = FreeType.freetype_init ()
let () = FreeType.freetype_load_font ()
let () = FreeType.freetype_set_pixel_sizes 6

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
;;
let w = match
  sdl_create_window "limitless" 0 0 800 800
    (sdl_window_resizable lor sdl_window_allow_highdpi)
with
| Some (Window { width; height; title; _ } as w) ->
    Printf.printf "Created window: %s %d %d" title width height;
    print_newline ();
    w
| None -> failwith "unable to create window"
;;
    sdl_create_renderer w sdl_renderer_software;
    sdl_set_render_draw_blendmode w sdl_blendmode_blend

let () = sdl_set_render_draw_color w 255 255 255 255;;

let draw_rect () =
    let rect = Rect { x = 0; y = 0; width = 250; height = 300 } in
    sdl_set_render_draw_color w 255 0 0 255;
    sdl_renderer_draw_rect w rect;
    sdl_renderer_fill_rect w rect

let draw_points points =
    sdl_set_render_draw_color w 0 255 0 200;
    sdl_render_draw_points w points

let points =
  let rec points' x y acc =
    let new_point = Point (x, y) in
    match (x, y) with
    | 100, 100 -> new_point :: acc
    | _, 100 -> points' (succ x) 0 (new_point :: acc)
    | _ -> points' x (succ y) (new_point :: acc)
  in
  points' 0 0 []

let draw_bmp_points glyph_info pts offset =
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
  (*draw_glyphs ();*)
  sdl_render_present w

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
         ( Int.to_float (x + g.FreeType.metrics.horiBearingX),
           Int.to_float y
           +. Int.to_float biggest_horiBearingY
           -. Int.to_float g.FreeType.metrics.horiBearingY );
       sdl_render_present w);
      g.FreeType.advance
  | None -> (0, 0)

let erase_letter_glyph letter (x, y) =
  let found_glyph =
    List.find_opt (fun (c, _) -> Char.chr c = letter) glyph_infos
  in
  match found_glyph with
  | Some (_, g) -> (
        let rect =
          RectF
            {
              x = Int.to_float x -. Int.to_float (fst g.FreeType.advance);
              y =
                Int.to_float y
                +. Int.to_float biggest_horiBearingY
                -. Int.to_float g.FreeType.metrics.horiBearingY;
              width =
                Int.to_float g.FreeType.metrics.width
                +. Int.to_float g.FreeType.metrics.horiBearingX;
              height = Int.to_float g.FreeType.metrics.height;
            }
        in
        sdl_set_render_draw_color w 0 0 0 255;
        sdl_renderer_draw_rect_float w rect;
        sdl_renderer_fill_rect_float w rect;
        sdl_render_present w;
        (-fst g.FreeType.advance, 0))
  | None -> (0, 0)

type editor_info = { buffer : string; cursor_pos : int * int }

let rec loop editor_info =
  let evt = sdl_pollevent () in
  let new_buffer, (offset, continue) =
    match evt with
    | Some (KeyboardEvt { keysym; timestamp; _ }) ->
        Printf.printf "KBD: %d, %d" (Char.code keysym) timestamp;
        print_newline ();
        let char_code = Char.code keysym in
        let str_len = String.length editor_info.buffer in
        if char_code = 8 && str_len > 0 then
          let cursor_offset = erase_letter_glyph editor_info.buffer.[str_len - 1] editor_info.cursor_pos in
              (String.sub editor_info.buffer 0 (str_len - 1), (cursor_offset, true))
        else
      (editor_info.buffer, ((0, 0), true))
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
        (editor_info.buffer, ((0, 0), true))
    | Some (WindowEvt { event; _ }) -> (
        match event with
        | WindowClose -> (editor_info.buffer, ((0, 0), false))
        | WindowResize -> (editor_info.buffer, ((0, 0), true)))
    | Some (MouseMotionEvt { x; y; _ }) ->
        Printf.printf "Mousemotion %d %d" x y;
        print_newline ();
        (editor_info.buffer, ((0, 0), true))
    | Some (TextInputEvt { text; _ }) -> (
      let new_cursor_pos = String.fold_right (fun c acc -> draw_letter_glyph c acc) text editor_info.cursor_pos in
      (editor_info.buffer ^ text, (new_cursor_pos, true))
    )
    | Some Quit -> (editor_info.buffer, ((0, 0), false))
    | None -> (editor_info.buffer, ((0, 0), true))
  in
  let x, y = editor_info.cursor_pos in
  if continue then
    loop { buffer = new_buffer; cursor_pos = (x + fst offset, y + snd offset) }
  else ()

let _ = loop { buffer = ""; cursor_pos = (0, 0) }
