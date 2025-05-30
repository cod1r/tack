open Tack.Sdl
open Tack.Editor
open Tack.Rope
open Tack.Render
open Tack.Modes

let s = Another.hi
let () = print_endline s

let rec loop (editor_info : Editor.editor) =
  let evt = Sdl.sdl_waitevent () in
  let new_editor, continue =
    match evt with
    | Some Quit -> (editor_info, false)
    | None -> (editor_info, true)
    | _ -> (
        match editor_info.current_rope_idx with
        | Some idx ->
            let rope = List.nth editor_info.ropes idx in
            let editor, continue =
              match rope with
              | File _ -> (FileMode.handle_mode_evt editor_info evt, true)
              | FileSearch _ ->
                  (FileSearchMode.handle_mode_evt editor_info evt, true)
            in
            Render.draw editor;
            (editor, continue)
        | None -> (editor_info, true))
  in
  if continue then loop new_editor else ()

let cwd = Sys.getcwd ()
and filename = Sys.argv.(1)

let real_path = cwd ^ "/" ^ filename

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
    Editor.default_editor with
    ropes =
      [
        File
          {
            rope = Some file_rope;
            cursor_pos = 0;
            file_name = real_path;
            last_modification_time = 0.;
            vertical_scroll_y_offset = 0;
            highlight = None;
          };
      ];
    (* [ FileSearch { search_rope = None; cursor_pos = 0; results = [] } ]; *)
    current_rope_idx = Some 0;
  }

let () = Render.setup_glyph_texture ~editor:initial_editor
let () = Render.draw initial_editor
let () = Sdl.sdl_create_and_set_system_cursor ()
let () = loop initial_editor
