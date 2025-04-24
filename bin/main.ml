open Tack.Sdl
open Tack.Editor
open Tack.Rope
open Tack.Render

let _ = "URMOMS HAHA"

let cwd = Sys.getcwd ()
and filename = Sys.argv.(1)

let real_path = cwd ^ "/" ^ filename

let rec loop (editor_info : Editor.editor) =
  let evt = Sdl.sdl_pollevent () in
  let new_editor, continue =
    match evt with
    | Some (KeyboardEvt { keysym; timestamp; kbd_evt_type; _ }) -> (
        Printf.printf "KBD: %d, %s, %d" (Char.code keysym) (Char.escaped keysym)
          timestamp;
        print_newline ();
        let char_code = Char.code keysym in
        match editor_info.Editor.rope with
        | Some r -> (
            match char_code with
            | 8 when kbd_evt_type = Keydown ->
                let rope_len = length r in
                if rope_len > 0 && editor_info.cursor_pos > 0 then (
                  let new_rope =
                    Some (delete r (editor_info.cursor_pos - 1) 1)
                  in
                  let new_editor : Editor.editor =
                    {
                      editor_info with
                      rope = new_rope;
                      cursor_pos = editor_info.cursor_pos - 1;
                    }
                  in
                  Render.draw new_editor;
                  (new_editor, true))
                else (editor_info, true)
            | 9 ->
                (* horizontal tab will be two spaces *)
                let new_rope = Some (insert r editor_info.cursor_pos "  ") in
                let new_editor : Editor.editor =
                  {
                    editor_info with
                    rope = new_rope;
                    cursor_pos = editor_info.cursor_pos + 1;
                  }
                in
                Render.draw new_editor;
                (new_editor, true)
            | 13 | 10 ->
                (* on macos, the return key gives \r instead of \n *)
                let new_rope = Some (insert r editor_info.cursor_pos "\n") in
                let new_editor : Editor.editor =
                  {
                    editor_info with
                    rope = new_rope;
                    cursor_pos = editor_info.cursor_pos + 1;
                  }
                in
                Render.draw new_editor;
                (new_editor, true)
            | 1073742048 ->
                (* this is the integer encoding for ctrl in SDL *)
                let new_editor =
                  match kbd_evt_type with
                  | Keydown -> { editor_info with holding_ctrl = true }
                  | Keyup -> { editor_info with holding_ctrl = false }
                in
                (new_editor, true)
            | 115 ->
                (match editor_info.holding_ctrl with
                | true -> (
                    match editor_info.rope with
                    | Some r ->
                        Out_channel.with_open_bin real_path (fun oc ->
                            Out_channel.output_string oc (to_string r))
                    | None -> ())
                | false -> ());
                (editor_info, true)
            | _ -> (editor_info, true))
        | None -> (editor_info, true))
    | Some
        (MouseButtonEvt
           { mouse_evt_type; timestamp; x; y; windowID; button; clicks }) -> (
        match mouse_evt_type with
        | Mousedown ->
            Printf.printf "Mousedown %d, %d, %d, %d, %d, %d\n" x y windowID
              button clicks timestamp;
            let crp =
              if Option.is_some editor_info.rope then
                Editor.find_closest_rope_pos_for_cursor_on_mouse_down
                  editor_info (x, y)
              else 0
            in
            Printf.printf "closest rp: %d" crp;
            print_newline ();
            let new_editor = { editor_info with cursor_pos = crp } in
            Render.draw new_editor;
            (new_editor, true)
        | Mouseup ->
            Printf.printf "Mouseup";
            print_newline ();
            (editor_info, true))
    | Some (WindowEvt { event; _ }) -> (
        match event with
        | WindowClose -> (editor_info, false)
        | WindowResize ->
            Render.draw editor_info;
            (editor_info, true))
    | Some (MouseMotionEvt { x = _; _ }) ->
        (* Printf.printf "Mousemotion %d %d %d" x y timestamp;
        print_newline (); *)
        (editor_info, true)
    | Some (MouseWheelEvt { y; _ }) ->
        let new_editor =
          {
            editor_info with
            vertical_scroll_y_offset = editor_info.vertical_scroll_y_offset + y;
          }
        in
        Render.draw new_editor;
        (new_editor, true)
    | Some (TextInputEvt { text; _ }) ->
        let new_rope =
          match editor_info.Editor.rope with
          | Some r -> Some (insert r editor_info.cursor_pos text)
          | None -> Some (Leaf text)
        in
        let new_editor : Editor.editor =
          {
            editor_info with
            rope = new_rope;
            cursor_pos = editor_info.cursor_pos + 1;
          }
        in
        Render.draw new_editor;
        (new_editor, true)
    | Some Quit -> (editor_info, false)
    | None -> (editor_info, true)
  in
  if continue then loop new_editor else ()

let () =
  if not (real_path |> Sys.file_exists) then
    failwith ("File doesn't exist: " ^ cwd ^ filename)

let file_contents =
  let lines =
    In_channel.with_open_bin real_path (fun ic -> In_channel.input_lines ic)
  in
  List.fold_left (fun acc line -> acc ^ line ^ "\n") "" lines

let file_rope = of_string file_contents |> rebalance

let initial_editor : Editor.editor =
  {
    rope = Some file_rope;
    cursor_pos = 0;
    holding_ctrl = false;
    vertical_scroll_y_offset = 0;
  }

let () = Render.draw initial_editor
let () = Sdl.sdl_create_and_set_system_cursor ()
let () = loop initial_editor
