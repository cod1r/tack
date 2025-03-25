open Limitless.Sdl
open Limitless.Freetype
open Limitless.Editor
open Limitless.Rope
open Limitless.Render
open Limitless.Opengl;;

match Sdl.init_sdl () with
| Ok(()) -> ()
| Error e -> failwith e;;

let () = FreeType.freetype_init ()
let () = FreeType.freetype_load_font ()
let () = FreeType.freetype_set_pixel_sizes 6

let glyph_infos =
  let startcode, endcode = (32, 126) in
  let rec get_glyph_info char_code acc =
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

let w =
  match
    Sdl.sdl_create_window "limitless" 0 0 800 800
      sdl_window_opengl
  with
  | Some (Window { width; height; title; _ } as w) ->
      Printf.printf "Created window: %s %d %d" title width height;
      print_newline ();
      w
  | None -> failwith "unable to create window"
;;

match Sdl.sdl_gl_create_context w with
| Ok(()) -> ()
| Error e -> failwith e
;;

match Sdl.sdl_gl_make_current w with
| Ok(()) -> ()
| Error e -> failwith e

let bigarray = Bigarray.Array1.create Float32 C_layout 10_000;;

let rec loopTestOpenGL () =
  let evt = Sdl.sdl_pollevent () in ();
  let continue = match evt with
  | Some Quit -> false
  | _ -> true in
  gl_clear_color 0. 1. 0. 1.;
  gl_clear ();
  (match Sdl.sdl_gl_swapwindow w with
  | Ok(()) -> ()
  | Error e -> failwith e);
  if continue then loopTestOpenGL () else ()
;;

loopTestOpenGL ();;

let rec loop editor_info =
  Render.draw bigarray editor_info.Editor.rope biggest_horiBearingY;
  let evt = Sdl.sdl_pollevent () in
  let new_editor, continue =
    match evt with
    | Some (KeyboardEvt { keysym; timestamp; _ }) -> (
        Printf.printf "KBD: %d, %d" (Char.code keysym) timestamp;
        print_newline ();
        let char_code = Char.code keysym in
        match editor_info.Editor.rope with
        | Some r ->
            let rope_len = length r in
            if char_code = 8 && rope_len > 0 then
              ( {
                  Editor.rope = Some (delete r (length r - 1) 1);
                  cursor_pos = editor_info.cursor_pos;
                },
                true )
            else (editor_info, true)
        | None -> (editor_info, true))
    | Some
        (MouseButtonEvt
           { mouse_evt_type; timestamp; x; y; windowID; button; clicks }) ->
        (match mouse_evt_type with
        | Mousedown -> (
            Printf.printf "Mousedown %d, %d, %d, %d, %d, %d" x y windowID button
              clicks timestamp;
            print_newline ();
            match editor_info.rope with
            | Some r ->
                let closest =
                  Editor.search_closest (Int.to_float x, Int.to_float y) r
                in
                Printf.printf "closest: %f %f" (fst closest) (snd closest);
                print_newline ();
                Render.draw_cursor w closest biggest_horiBearingY
            | None -> ())
        | Mouseup ->
            Printf.printf "Mouseup";
            print_newline ());
        (editor_info, true)
    | Some (WindowEvt { event; _ }) -> (
        match event with
        | WindowClose -> (editor_info, false)
        | WindowResize -> (editor_info, true))
    | Some (MouseMotionEvt { x; y; _ }) ->
        Printf.printf "Mousemotion %d %d" x y;
        print_newline ();
        (editor_info, true)
    | Some (TextInputEvt { text; _ }) -> (
        let char_list = String.fold_left (fun acc c -> c :: acc) [] text in
        let zipped =
          List.map
            (fun c ->
              let _, gi =
                List.find (fun (c', _) -> c = Char.chr c') glyph_infos
              in
              (c, gi))
            char_list
        in
        match editor_info.rope with
        | Some r ->
            ( {
                rope = Some (concat r (Leaf zipped));
                cursor_pos = editor_info.cursor_pos;
              },
              true )
        | None ->
            ( { rope = Some (Leaf zipped); cursor_pos = editor_info.cursor_pos },
              true ))
    | Some Quit -> (editor_info, false)
    | None -> (editor_info, true)
  in
  if continue then loop new_editor else ()

(*let _ = loop { rope = None; cursor_pos = (0, 0) }*)
