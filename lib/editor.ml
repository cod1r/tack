open Sdl

type file = {
  rope : Rope.rope option;
  cursor_pos : int;
  file_name : string;
  scroll_y_offset : int;
  last_modification_time : float;
  highlight : (int * int) option;
}

type editor = {
  ropes : file list;
  holding_ctrl : bool;
  current_rope_idx : int option;
  bounds : Ui.bounding_box;
}

let open_file file_name =
  Printf.printf "Trying to open %s" file_name;
  print_newline ();
  In_channel.with_open_bin file_name (fun ic -> In_channel.input_all ic)
  |> Rope.of_string

let config_has_been_modified_during_runtime () =
  let s = Unix.stat ".config.json" in
  Unix.time () -. s.st_mtime < 1.

let default_editor : editor =
  let width, height = Sdl.sdl_gl_getdrawablesize () in
  {
    ropes = [];
    holding_ctrl = false;
    current_rope_idx = None;
    bounds = { width; height; x = 0; y = 0 };
  }
