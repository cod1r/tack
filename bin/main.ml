open Limitless.Sdl
open Limitless.Freetype
open Limitless.Editor
open Limitless.Rope
open Limitless.Render
open Limitless.Opengl

let rec loop editor_info =
  Render.draw editor_info.Editor.rope;
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
            match editor_info.rope with Some r -> () | None -> ())
        | Mouseup ->
            Printf.printf "Mouseup";
            print_newline ());
        (editor_info, true)
    | Some (WindowEvt { event; _ }) -> (
        match event with
        | WindowClose -> (editor_info, false)
        | WindowResize -> (editor_info, true))
    | Some (MouseMotionEvt { x; y; timestamp; _ }) ->
        Printf.printf "Mousemotion %d %d %d" x y timestamp;
        print_newline ();
        (editor_info, true)
    | Some (TextInputEvt { text; _ }) -> (
        match editor_info.Editor.rope with
        | Some r ->
            ( {
                Editor.rope = Some (concat r (Leaf text));
                cursor_pos = editor_info.cursor_pos;
              },
              true )
        | None ->
            ( {
                Editor.rope = Some (Leaf text);
                cursor_pos = editor_info.cursor_pos;
              },
              true ))
    | Some Quit -> (editor_info, false)
    | None -> (editor_info, true)
  in
  if continue then loop new_editor else ()

let _ = loop { rope = None; cursor_pos = (0, 0) }
