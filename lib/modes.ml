open Render
open Sdl
open Editor

module EditingMode = struct
  (* function i made in the process of thinking of a good answer to different editor modes (file search mode, editing mode, etc; this probably isn't needed *)
  let handle_kbd_evt_editor_mode (editor_info : Editor.editor) ~char_code
      ~kbd_evt_type ~keysym ~real_path =
    match editor_info.rope with
    | Some r -> (
        match char_code with
        | 1073742048 ->
            (* this is the integer encoding for ctrl in SDL *)
            let new_editor =
              match kbd_evt_type with
              | Sdl.Keydown -> { editor_info with holding_ctrl = true }
              | Keyup -> { editor_info with holding_ctrl = false }
            in
            new_editor
        | _ -> (
            match keysym with
            | '\b' when kbd_evt_type = Keydown ->
                (* backspace *)
                let rope_len = Rope.length r in
                if rope_len > 0 && editor_info.cursor_pos > 0 then (
                  let new_rope =
                    Some (Rope.delete r (editor_info.cursor_pos - 1) 1)
                  in
                  let new_editor : Editor.editor =
                    {
                      editor_info with
                      rope = new_rope;
                      cursor_pos = editor_info.cursor_pos - 1;
                    }
                  in
                  Render.draw new_editor;
                  new_editor)
                else editor_info
            | 'c' when kbd_evt_type = Keydown ->
                (match (editor_info.rope, editor_info.highlight) with
                | Some r, Some (start, end') ->
                    Rope.substring r ~start ~len:(end' - start)
                    |> Rope.to_string |> Sdl.set_clipboard_text
                | _ -> ());
                editor_info
            | 'v' when kbd_evt_type = Keydown -> (
                match editor_info.holding_ctrl with
                | true ->
                    let clipboard_contents = Sdl.get_clipboard_text () in
                    print_string clipboard_contents;
                    print_newline ();
                    let new_rope =
                      match editor_info.rope with
                      | Some r ->
                          Rope.insert r editor_info.cursor_pos
                            clipboard_contents
                      | None -> Leaf clipboard_contents
                    in
                    let new_editor =
                      {
                        editor_info with
                        rope = Some new_rope;
                        cursor_pos =
                          editor_info.cursor_pos
                          + String.length clipboard_contents;
                      }
                    in
                    Render.draw new_editor;
                    new_editor
                | false -> editor_info)
            | 's' when kbd_evt_type = Keydown ->
                (match editor_info.holding_ctrl with
                | true -> (
                    match editor_info.rope with
                    | Some r ->
                        Out_channel.with_open_bin real_path (fun oc ->
                            Out_channel.output_string oc (Rope.to_string r))
                    | None -> ())
                | false -> ());
                editor_info
            | ('\r' | '\n') when kbd_evt_type = Keydown ->
                (* on macos, the return key gives \r instead of \n *)
                let new_rope =
                  Some (Rope.insert r editor_info.cursor_pos "\n")
                in
                let new_editor : Editor.editor =
                  {
                    editor_info with
                    rope = new_rope;
                    cursor_pos = editor_info.cursor_pos + 1;
                  }
                in
                Render.draw new_editor;
                new_editor
            | '\t' when kbd_evt_type = Keydown ->
                (* horizontal tab will be two spaces *)
                let new_rope =
                  Some (Rope.insert r editor_info.cursor_pos "  ")
                in
                let new_editor : Editor.editor =
                  {
                    editor_info with
                    rope = new_rope;
                    cursor_pos = editor_info.cursor_pos + 2;
                  }
                in
                Render.draw new_editor;
                new_editor
            | _ -> editor_info))
    | None -> editor_info

  let handle_editing_mode_evt (editor : Editor.editor) (evt : Sdl.event option)
      =
    match evt with
    | Some (KeyboardEvt { keysym; kbd_evt_type; _ }) -> (
        Printf.printf "KBD: %d, %s" (Char.code keysym) (Char.escaped keysym);
        print_newline ();
        let char_code = Char.code keysym in
        match editor.mode with
        | Editing { real_path } ->
            handle_kbd_evt_editor_mode editor ~char_code ~kbd_evt_type ~keysym
              ~real_path
        | _ -> editor)
    | Some
        (MouseButtonEvt
           { mouse_evt_type; timestamp; x; y; windowID; button; clicks }) -> (
        match mouse_evt_type with
        | Mousedown ->
            Printf.printf "Mousedown %d, %d, %d, %d, %d, %d\n" x y windowID
              button clicks timestamp;
            let crp =
              if Option.is_some editor.rope then
                Editor.find_closest_rope_pos_for_cursor_on_coords editor (x, y)
              else 0
            in
            Printf.printf "closest rp: %d" crp;
            print_newline ();
            let new_editor = { editor with cursor_pos = crp } in
            Render.draw new_editor;
            new_editor
        | Mouseup ->
            Printf.printf "Mouseup: %d %d" x y;
            print_newline ();
            let crp =
              if Option.is_some editor.rope then
                Editor.find_closest_rope_pos_for_cursor_on_coords editor (x, y)
              else 0
            in
            let new_editor =
              {
                editor with
                cursor_pos = crp;
                highlight =
                  (if crp != editor.cursor_pos then
                     Some (min crp editor.cursor_pos, max crp editor.cursor_pos)
                   else None);
              }
            in
            Render.draw new_editor;
            new_editor)
    | Some (WindowEvt { event; _ }) -> (
        match event with
        | WindowClose -> editor
        | WindowResize ->
            Render.draw editor;
            editor
        | Unhandled -> editor)
    | Some (MouseMotionEvt { x; y; _ }) ->
        (* Printf.printf "Mousemotion %d %d" x y; *)
        (* print_newline (); *)
        editor
    | Some (MouseWheelEvt { y; _ }) ->
        let new_editor =
          {
            editor with
            vertical_scroll_y_offset = editor.vertical_scroll_y_offset + y;
          }
        in
        Render.draw new_editor;
        new_editor
    | Some (TextInputEvt { text; _ }) ->
        let new_rope =
          match editor.Editor.rope with
          | Some r -> Some (Rope.insert r editor.cursor_pos text)
          | None -> Some (Leaf text)
        in
        let new_editor : Editor.editor =
          { editor with rope = new_rope; cursor_pos = editor.cursor_pos + 1 }
        in
        Render.draw new_editor;
        new_editor
    | _ -> editor
end
