open Tack.Sdl
open Tack.Freetype
open Tack.Editor
open Tack.Rope
open Tack.Render
open Tack.Opengl

let rec loop editor_info =
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
        | Mousedown ->
            Printf.printf "Mousedown %d, %d, %d, %d, %d, %d" x y windowID button
              clicks timestamp;
            print_newline ()
        | Mouseup ->
            Printf.printf "Mouseup";
            print_newline ());
        (editor_info, true)
    | Some (WindowEvt { event; _ }) -> (
        match event with
        | WindowClose -> (editor_info, false)
        | WindowResize -> (editor_info, true))
    | Some (MouseMotionEvt { x; y; timestamp; _ }) ->
        (* Printf.printf "Mousemotion %d %d %d" x y timestamp;
        print_newline (); *)
        (editor_info, true)
    | Some (TextInputEvt { text; _ }) ->
        let new_rope =
          match editor_info.Editor.rope with
          | Some r -> Some (concat r (Leaf text))
          | None -> Some (Leaf text)
        in
        Render.draw new_rope;
        ({ Editor.rope = new_rope; cursor_pos = editor_info.cursor_pos }, true)
    | Some Quit -> (editor_info, false)
    | None -> (editor_info, true)
  in
  if continue then loop new_editor else ()
;;

Render.draw None

let _ = loop { rope = None; cursor_pos = (0, 0) }
