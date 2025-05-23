open Freetype
open Rope
open Sdl

let _LINE_NUMBER_RIGHT_PADDING = 20

module Editor = struct
  type information_relating_to_config = {
    glyph_info_with_char : (char * FreeType.glyph_info) Array.t;
    ft_face : FreeType.ft_face;
    pixel_size : int;
    font_height : int;
  }

  type rope_wrapper =
    | FileSearch of {
        search_rope : Rope.rope option;
        cursor_pos : int;
        results : string list;
      }
    | File of {
        rope : Rope.rope option;
        cursor_pos : int;
        file_name : string;
        vertical_scroll_y_offset : int;
        last_modification_time : float;
        highlight : (int * int) option;
      }

  type editor = {
    ropes : rope_wrapper list;
    holding_ctrl : bool;
    config_info : information_relating_to_config;
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

  let read_config () =
    let config_str =
      In_channel.with_open_bin ".config.json" (fun ic ->
          In_channel.input_all ic)
    in
    Yojson.Safe.from_string config_str

  let recalculate_info_relating_to_config () : information_relating_to_config =
    let config = read_config () in
    let font_pixel_size =
      Yojson.Safe.Util.member "font_pixel_size" config
      |> Yojson.Safe.Util.to_int
    and font_path =
      Yojson.Safe.Util.member "font_path" config |> Yojson.Safe.Util.to_string
    in
    let face = FreeType.freetype_get_face font_path FreeType.library in
    FreeType.freetype_set_pixel_sizes face font_pixel_size;
    (* need to call font_height after set_pixel_sizes *)
    let font_height = FreeType.get_font_height face in
    {
      glyph_info_with_char =
        Array.init
          (126 - 32 + 1)
          (fun i -> FreeType.get_ascii_char_glyph face (i + 32));
      ft_face = face;
      pixel_size = font_pixel_size;
      font_height;
    }

  let default_editor : editor =
    {
      ropes = [];
      holding_ctrl = false;
      config_info = recalculate_info_relating_to_config ();
      current_rope_idx = None;
      bounds = { width = 0; height = 0; x = 0; y = 0 };
    }

  let rec traverse_rope (rope : Rope.rope) (handle_result : 'a -> char -> 'a)
      (result : 'a) =
    match rope with
    | Leaf l -> String.fold_left handle_result result l
    | Node { left; right; _ } ->
        let left_result = traverse_rope left handle_result result in
        traverse_rope right handle_result left_result

  let num_lines (rope : Rope.rope) =
    let fold_fn (acc : int) ch = if ch = '\n' then acc + 1 else acc in
    let accumulation = traverse_rope rope fold_fn 0 in
    accumulation

  let get_digits_widths_summed (num_lines : int) (editor : editor) =
    string_of_int num_lines
    |> String.fold_left
         (fun acc c ->
           (Array.find_opt
              (fun (c', _) -> c' = c)
              editor.config_info.glyph_info_with_char
           |> Option.get)
           :: acc)
         []
    |> List.fold_left (fun acc (_, gi) -> acc + FreeType.get_x_advance gi) 0
    |> fun dws -> dws + _LINE_NUMBER_RIGHT_PADDING

  type rope_traversal_info = { x : int; y : int; rope_pos : int }

  let find_closest_rope_pos_for_cursor_on_coords (editor : editor)
      ((x, y) : int * int) =
    let window_width, _ = Sdl.sdl_gl_getdrawablesize () in
    let window_width_without_high_dpi, _ = Sdl.sdl_get_window_size Sdl.w in
    (* ratio is needed because the x,y coords given from MouseEvent is based on window without high dpi so scaling needs to happen *)
    let ratio = window_width / window_width_without_high_dpi in
    let fold_fn (traversal_info : rope_traversal_info) c = traversal_info in
    let current_rope =
      List.nth editor.ropes (editor.current_rope_idx |> Option.get)
    in
    match current_rope with
    | File { rope; vertical_scroll_y_offset; _ } ->
        let _ =
          traverse_rope (rope |> Option.get) fold_fn
            ({ x = 0; y = 0; rope_pos = 0 } : rope_traversal_info)
        in
        0
    | _ -> failwith "NOT FILE"
end
