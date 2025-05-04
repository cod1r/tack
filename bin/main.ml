open Tack.Sdl
open Tack.Editor
open Tack.Rope
open Tack.Render
open Tack.Modes

let cwd = Sys.getcwd ()
and filename = Sys.argv.(1)

let real_path = cwd ^ "/" ^ filename

let rec loop (editor_info : Editor.editor) =
  let evt = Sdl.sdl_pollevent () in
  let new_editor, continue =
    match evt with
    | Some Quit -> (editor_info, false)
    | None -> (editor_info, true)
    | _ -> (
        match editor_info.mode with
        | Editing { real_path = _ } ->
            (EditingMode.handle_editing_mode_evt editor_info evt, true)
        | _ -> (editor_info, true))
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
  { Editor.default_editor with rope = Some file_rope }

let () = Render.draw initial_editor
let () = Sdl.sdl_create_and_set_system_cursor ()
let () = loop initial_editor
(* HAHA THIS IS FUN *)
