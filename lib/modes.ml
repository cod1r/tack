open Render
open Sdl
open Editor

module type Mode = sig
  val handle_mode_evt : Editor.editor -> Sdl.event option -> Editor.editor
end

module FileMode: Mode = struct
  (* function i made in the process of thinking of a good answer to different editor modes (file search mode, editing mode, etc; this probably isn't needed *)
  let handle_kbd_evt_editor_mode (editor : Editor.editor) ~char_code
      ~kbd_evt_type ~keysym ~file_path =
    let rope_wrapper =
      List.nth editor.ropes (editor.current_rope_idx |> Option.get)
    in
    match rope_wrapper with
    | File { rope; cursor_pos; file_name } -> (
        let other_rope_wrappers =
          List.filteri
            (fun idx _ -> idx != (editor.current_rope_idx |> Option.get))
            editor.ropes
        in
        match rope with
        | Some r -> (
            match char_code with
            | 1073742048 -> (
                (* this is the integer encoding for ctrl in SDL *)
                match kbd_evt_type with
                | Sdl.Keydown -> { editor with holding_ctrl = true }
                | Keyup -> { editor with holding_ctrl = false })
            | _ -> (
                match keysym with
                | '\b' when kbd_evt_type = Keydown ->
                    (* backspace *)
                    let rope_len = Rope.length r in
                    if rope_len > 0 && cursor_pos > 0 then (
                      let new_rope = Some (Rope.delete r (cursor_pos - 1) 1) in
                      let new_rope_wrapper =
                        Editor.File
                          {
                            file_name;
                            cursor_pos = cursor_pos - 1;
                            rope = new_rope;
                          }
                      in
                      let new_editor : Editor.editor =
                        {
                          editor with
                          ropes = new_rope_wrapper :: other_rope_wrappers;
                          current_rope_idx = Some 0;
                        }
                      in
                      Render.draw new_editor;
                      new_editor)
                    else editor
                | 'c' when kbd_evt_type = Keydown ->
                    (match (rope, editor.highlight) with
                    | Some r, Some (start, end') ->
                        Rope.substring r ~start ~len:(end' - start)
                        |> Rope.to_string |> Sdl.set_clipboard_text
                    | _ -> ());
                    editor
                | 'v' when kbd_evt_type = Keydown -> (
                    match editor.holding_ctrl with
                    | true ->
                        let clipboard_contents = Sdl.get_clipboard_text () in
                        let new_rope =
                          match rope with
                          | Some r ->
                              Some (Rope.insert r cursor_pos clipboard_contents)
                          | None -> Some (Leaf clipboard_contents)
                        in
                        let new_rope_wrapper =
                          Editor.File
                            {
                              file_name;
                              cursor_pos =
                                cursor_pos + String.length clipboard_contents;
                              rope = new_rope;
                            }
                        in
                        let new_editor =
                          {
                            editor with
                            ropes = new_rope_wrapper :: other_rope_wrappers;
                            current_rope_idx = Some 0;
                          }
                        in
                        Render.draw new_editor;
                        new_editor
                    | false -> editor)
                | 's' when kbd_evt_type = Keydown ->
                    (match editor.holding_ctrl with
                    | true -> (
                        match rope with
                        | Some r ->
                            Out_channel.with_open_bin file_path (fun oc ->
                                Out_channel.output_string oc (Rope.to_string r))
                        | None -> ())
                    | false -> ());
                    editor
                | ('\r' | '\n') when kbd_evt_type = Keydown ->
                    (* on macos, the return key gives \r instead of \n *)
                    let new_rope = Some (Rope.insert r cursor_pos "\n") in
                    let new_rope_wrapper =
                      Editor.File
                        {
                          file_name;
                          cursor_pos = cursor_pos + 1;
                          rope = new_rope;
                        }
                    in
                    let new_editor : Editor.editor =
                      {
                        editor with
                        ropes = new_rope_wrapper :: other_rope_wrappers;
                        current_rope_idx = Some 0;
                      }
                    in
                    Render.draw new_editor;
                    new_editor
                | '\t' when kbd_evt_type = Keydown ->
                    (* horizontal tab will be two spaces *)
                    let new_rope = Some (Rope.insert r cursor_pos "  ")
                    and new_ropes =
                      List.filteri
                        (fun idx _ ->
                          idx != (editor.current_rope_idx |> Option.get))
                        editor.ropes
                    in
                    let new_rope_wrapper =
                      Editor.File
                        {
                          file_name;
                          cursor_pos = cursor_pos + 2;
                          rope = new_rope;
                        }
                    in
                    let new_editor : Editor.editor =
                      {
                        editor with
                        ropes = new_rope_wrapper :: new_ropes;
                        current_rope_idx = Some 0;
                      }
                    in
                    Render.draw new_editor;
                    new_editor
                | _ -> editor))
        | None -> editor)
    | _ -> editor

  let handle_mode_evt (editor : Editor.editor) (evt : Sdl.event option)
      =
    let current_rope_wrapper =
      List.nth editor.ropes (editor.current_rope_idx |> Option.get)
    in
    match current_rope_wrapper with
    | File { rope; file_name; cursor_pos; _ } -> (
        let other_rope_wrappers =
          List.filteri
            (fun idx _ -> idx != (editor.current_rope_idx |> Option.get))
            editor.ropes
        in
        match evt with
        | Some (KeyboardEvt { keysym; kbd_evt_type; _ }) ->
            Printf.printf "KBD: %d, %s" (Char.code keysym) (Char.escaped keysym);
            print_newline ();
            let char_code = Char.code keysym in
            handle_kbd_evt_editor_mode editor ~char_code ~kbd_evt_type ~keysym
              ~file_path:file_name
        | Some
            (MouseButtonEvt
               { mouse_evt_type; timestamp; x; y; windowID; button; clicks })
          -> (
            match mouse_evt_type with
            | Mousedown ->
                Printf.printf "Mousedown %d, %d, %d, %d, %d, %d\n" x y windowID
                  button clicks timestamp;
                let crp =
                  if Option.is_some rope then
                    Editor.find_closest_rope_pos_for_cursor_on_coords editor
                      (x, y)
                  else 0
                in
                Printf.printf "closest rp: %d" crp;
                print_newline ();
                let new_rope_wrapper =
                  Editor.File { file_name; cursor_pos = crp; rope }
                in
                let new_editor =
                  {
                    editor with
                    ropes = new_rope_wrapper :: other_rope_wrappers;
                  }
                in
                Render.draw new_editor;
                new_editor
            | Mouseup ->
                Printf.printf "Mouseup: %d %d" x y;
                print_newline ();
                let crp =
                  if Option.is_some rope then
                    Editor.find_closest_rope_pos_for_cursor_on_coords editor
                      (x, y)
                  else 0
                in
                let new_rope_wrapper =
                  Editor.File { file_name; cursor_pos = crp; rope }
                in
                let new_editor =
                  {
                    editor with
                    ropes = new_rope_wrapper :: other_rope_wrappers;
                    highlight =
                      (if crp != cursor_pos then
                         Some (min crp cursor_pos, max crp cursor_pos)
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
        | Some (MouseMotionEvt _) ->
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
              match rope with
              | Some r -> Some (Rope.insert r cursor_pos text)
              | None -> Some (Leaf text)
            in
            let new_rope_wrapper =
              Editor.File
                { file_name; cursor_pos = cursor_pos + 1; rope = new_rope }
            in
            let new_editor : Editor.editor =
              { editor with ropes = new_rope_wrapper :: other_rope_wrappers }
            in
            Render.draw new_editor;
            new_editor
        | _ -> editor)
    | _ -> editor
end

module FileSearchMode: Mode = struct
  let handle_mode_evt (editor : Editor.editor) (evt : Sdl.event option)
      =
    let current_rope_wrapper =
      List.nth editor.ropes (editor.current_rope_idx |> Option.get)
    and other_rope_wrappers =
      List.filteri
        (fun idx _ -> idx != (editor.current_rope_idx |> Option.get))
        editor.ropes
    in
    match current_rope_wrapper with
    | FileSearch { rope; cursor_pos } -> (
        match evt with
        | Some (KeyboardEvt { keysym; kbd_evt_type; _ }) -> (
            Printf.printf "KBD: %d, %s" (Char.code keysym) (Char.escaped keysym);
            print_newline ();
            let char_code = Char.code keysym in
            match rope with
            | Some r -> (
                match char_code with
                | 1073742048 ->
                    (* this is the integer encoding for ctrl in SDL *)
                    let new_editor =
                      match kbd_evt_type with
                      | Sdl.Keydown -> { editor with holding_ctrl = true }
                      | Keyup -> { editor with holding_ctrl = false }
                    in
                    new_editor
                | _ -> (
                    match keysym with
                    | '\b' when kbd_evt_type = Keydown ->
                        (* backspace *)
                        let rope_len = Rope.length r in
                        if rope_len > 0 && cursor_pos > 0 then (
                          let new_rope =
                            Some (Rope.delete r (cursor_pos - 1) 1)
                          in
                          let new_rope_wrapper =
                            Editor.FileSearch
                              { rope = new_rope; cursor_pos = cursor_pos - 1 }
                          in
                          let new_editor : Editor.editor =
                            {
                              editor with
                              ropes = new_rope_wrapper :: other_rope_wrappers;
                              current_rope_idx = Some 0;
                            }
                          in
                          Render.draw new_editor;
                          new_editor)
                        else editor
                    | 'c' when kbd_evt_type = Keydown ->
                        (match (rope, editor.highlight) with
                        | Some r, Some (start, end') ->
                            Rope.substring r ~start ~len:(end' - start)
                            |> Rope.to_string |> Sdl.set_clipboard_text
                        | _ -> ());
                        editor
                    | 'v' when kbd_evt_type = Keydown -> (
                        match editor.holding_ctrl with
                        | true ->
                            let clipboard_contents =
                              Sdl.get_clipboard_text ()
                            in
                            print_string clipboard_contents;
                            print_newline ();
                            let new_rope =
                              match rope with
                              | Some r ->
                                  Rope.insert r cursor_pos clipboard_contents
                              | None -> Leaf clipboard_contents
                            in
                            let new_rope_wrapper =
                              Editor.FileSearch
                                {
                                  rope = Some new_rope;
                                  cursor_pos =
                                    cursor_pos
                                    + String.length clipboard_contents;
                                }
                            in
                            let new_editor =
                              {
                                editor with
                                ropes = new_rope_wrapper :: other_rope_wrappers;
                                current_rope_idx = Some 0;
                              }
                            in
                            Render.draw new_editor;
                            new_editor
                        | false -> editor)
                    | 's' when kbd_evt_type = Keydown -> editor
                    | ('\r' | '\n') when kbd_evt_type = Keydown -> editor
                    | '\t' when kbd_evt_type = Keydown ->
                        (* horizontal tab will be two spaces *)
                        let new_rope = Some (Rope.insert r cursor_pos "  ") in
                        let new_rope_wrapper =
                          Editor.FileSearch
                            { rope = new_rope; cursor_pos = cursor_pos + 2 }
                        in
                        let new_editor : Editor.editor =
                          {
                            editor with
                            ropes = new_rope_wrapper :: other_rope_wrappers;
                            current_rope_idx = Some 0;
                          }
                        in
                        Render.draw new_editor;
                        new_editor
                    | _ -> editor))
            | None -> editor)
        | Some
            (MouseButtonEvt
               { mouse_evt_type; timestamp; x; y; windowID; button; clicks })
          -> (
            match mouse_evt_type with
            | Mousedown ->
                Printf.printf "Mousedown %d, %d, %d, %d, %d, %d\n" x y windowID
                  button clicks timestamp;
                let crp =
                  if Option.is_some rope then
                    Editor.find_closest_rope_pos_for_cursor_on_coords editor
                      (x, y)
                  else 0
                in
                Printf.printf "closest rp: %d" crp;
                print_newline ();
                let new_rope_wrapper =
                  Editor.FileSearch { rope; cursor_pos = crp }
                in
                let new_editor =
                  {
                    editor with
                    ropes = new_rope_wrapper :: other_rope_wrappers;
                    current_rope_idx = Some 0;
                  }
                in
                Render.draw new_editor;
                new_editor
            | Mouseup ->
                Printf.printf "Mouseup: %d %d" x y;
                print_newline ();
                let crp =
                  if Option.is_some rope then
                    Editor.find_closest_rope_pos_for_cursor_on_coords editor
                      (x, y)
                  else 0
                in
                let new_rope_wrapper =
                  Editor.FileSearch { rope; cursor_pos = crp }
                in
                let new_editor =
                  {
                    editor with
                    ropes = new_rope_wrapper :: other_rope_wrappers;
                    current_rope_idx = Some 0;
                    highlight =
                      (if crp != cursor_pos then
                         Some (min crp cursor_pos, max crp cursor_pos)
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
              match rope with
              | Some r -> Some (Rope.insert r cursor_pos text)
              | None -> Some (Leaf text)
            in
            let new_rope_wrapper =
              Editor.FileSearch { rope = new_rope; cursor_pos = cursor_pos + 1 }
            in
            let new_editor : Editor.editor =
              {
                editor with
                ropes = new_rope_wrapper :: other_rope_wrappers;
                current_rope_idx = Some 0;
              }
            in
            Render.draw new_editor;
            new_editor
        | _ -> editor)
    | _ -> failwith "NOT FILE SEARCH"
end
