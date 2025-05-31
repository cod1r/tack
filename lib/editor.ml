open Freetype
open Rope
open Sdl

let _LINE_NUMBER_RIGHT_PADDING = 20

module Editor = struct
  type texture_atlas_info = { width : int; height : int; bytes : bytes }

  type information_relating_to_config = {
    other_glyph_info_with_char : (char * FreeType.glyph_info_) Array.t;
    glyph_info_with_char : (char * FreeType.glyph_info) Array.t;
    ft_face : FreeType.ft_face;
    pixel_size : int;
    font_height : int;
    descender : int;
    font_glyph_texture_atlas_info : texture_atlas_info;
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
    let descender = FreeType.get_descender face in
    let ascender = FreeType.get_ascender face in
    let other_glyph_info_with_char =
      Array.init
        (126 - 32 + 1)
        (fun i -> FreeType.get_ascii_char_glyph_info_ face (i + 32))
    in
    let widths_summed =
      Array.fold_left
        (fun acc (_, gi) -> acc + gi.FreeType.width)
        0 other_glyph_info_with_char
    in
    let global_font_height = ascender - descender in
    let bytes_texture_atlas =
      Bytes.init (widths_summed * global_font_height) (fun _ -> Char.chr 0)
    in
    (*
       the font glyph texture atlas is all of the glyphs that is loaded
       concatenated into a large bytes array

       widths of glyphs summed * font height
       ex:
         ABCDEF...
     *)
    let _ =
      Array.fold_left
        (fun current_width (c, gi) ->
          let gi_len = Bytes.length gi.FreeType.bytes in
          (if gi.width > 0 then
             let lst =
               List.init (gi_len / gi.width) (fun i ->
                   Bytes.init gi.width (fun i' ->
                       Bytes.get gi.bytes ((i * gi.width) + i')))
             in
             List.iteri
               (fun idx b ->
                 Bytes.blit b 0 bytes_texture_atlas
                   ((idx * widths_summed) + current_width)
                   (Bytes.length b))
               lst);
          current_width + gi.width)
        0 other_glyph_info_with_char
    in
    let font_glyph_texture_atlas_info =
      {
        width = widths_summed;
        height = global_font_height;
        bytes = bytes_texture_atlas;
      }
    in
    {
      other_glyph_info_with_char;
      glyph_info_with_char =
        Array.init
          (126 - 32 + 1)
          (fun i -> FreeType.get_ascii_char_glyph face (i + 32));
      ft_face = face;
      pixel_size = font_pixel_size;
      font_height;
      descender;
      font_glyph_texture_atlas_info;
    }

  let default_editor : editor =
    let width, height = Sdl.sdl_gl_getdrawablesize () in
    {
      ropes = [];
      holding_ctrl = false;
      config_info = recalculate_info_relating_to_config ();
      current_rope_idx = None;
      bounds = { width; height; x = 0; y = 0 };
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

  let get_digits_widths_summed ~(num_lines : int) ~(editor : editor) =
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

  type closest_information = {
    closest_col : int option;
    x : int;
    lower_y : int;
    upper_y : int;
    closest_vertical_range : (int * int) option;
    closest_rope : int;
    rope_pos : int;
  }

  let get_pair_col_and_rope_pos ~closest_info ~x =
    match closest_info.closest_vertical_range with
    | Some (s, e) ->
        if closest_info.lower_y = s && closest_info.upper_y = e then
          match closest_info.closest_col with
          | Some closest_col ->
              if abs (closest_col - x) < abs (closest_info.x - x) then
                (closest_info.closest_col, closest_info.closest_rope)
              else (Some closest_info.x, closest_info.rope_pos)
          | None -> (Some closest_info.x, closest_info.rope_pos)
        else (closest_info.closest_col, closest_info.closest_rope)
    | None -> (None, -1)

  let find_closest_vertical_range ~(editor : editor) ~rope ~digits_widths_summed
      ~y ~vertical_scroll_y_offset =
    let fold_fn_for_vertical_range (closest_info : closest_information) c =
      match c with
      | '\n' ->
          {
            closest_info with
            lower_y = closest_info.lower_y + editor.config_info.font_height;
            upper_y = closest_info.upper_y + editor.config_info.font_height;
            x = editor.bounds.x + digits_widths_summed;
            closest_vertical_range =
              (if y >= closest_info.lower_y && y <= closest_info.upper_y then
                 Some (closest_info.lower_y, closest_info.upper_y)
               else closest_info.closest_vertical_range);
          }
      | _ ->
          let _, gi =
            Array.find_opt
              (fun (c', _) -> c' = c)
              editor.config_info.glyph_info_with_char
            |> Option.get
          in
          let x_advance = FreeType.get_x_advance gi in
          let new_x, new_y =
            if
              closest_info.x + x_advance > editor.bounds.x + editor.bounds.width
            then
              ( editor.bounds.x + digits_widths_summed,
                closest_info.lower_y + editor.config_info.font_height )
            else (closest_info.x + x_advance, closest_info.lower_y)
          in
          {
            closest_info with
            x = new_x;
            lower_y = new_y;
            upper_y = new_y + editor.config_info.font_height;
            closest_vertical_range =
              (if y >= closest_info.lower_y && y <= closest_info.upper_y then
                 Some (closest_info.lower_y, closest_info.upper_y)
               else closest_info.closest_vertical_range);
          }
    in
    let lower_y =
      editor.bounds.y
      + (vertical_scroll_y_offset * editor.config_info.font_height)
    in
    let upper_y = lower_y + editor.config_info.font_height in
    let { closest_vertical_range; _ } : closest_information =
      traverse_rope rope fold_fn_for_vertical_range
        {
          lower_y;
          upper_y;
          closest_col = None;
          x = editor.bounds.x + digits_widths_summed;
          closest_rope = 0;
          rope_pos = 0;
          closest_vertical_range = None;
        }
    in
    closest_vertical_range

  let find_closest_horizontal_pos ~(editor : editor) ~rope ~x
      ~digits_widths_summed ~vertical_scroll_y_offset ~closest_vertical_range =
    let fold_fn_for_close_x (closest_info : closest_information) c =
      match c with
      | '\n' ->
          let closest_col, closest_rope =
            get_pair_col_and_rope_pos ~closest_info ~x
          in
          {
            closest_info with
            lower_y = closest_info.lower_y + editor.config_info.font_height;
            upper_y = closest_info.upper_y + editor.config_info.font_height;
            x = editor.bounds.x + digits_widths_summed;
            rope_pos = closest_info.rope_pos + 1;
            closest_col;
            closest_rope;
          }
      | _ ->
          let _, gi =
            Array.find_opt
              (fun (c', _) -> c' = c)
              editor.config_info.glyph_info_with_char
            |> Option.get
          in
          let x_advance = FreeType.get_x_advance gi in
          let closest_col, closest_rope =
            get_pair_col_and_rope_pos ~closest_info ~x
          in
          let new_x, new_y =
            if
              closest_info.x + x_advance > editor.bounds.x + editor.bounds.width
            then
              ( editor.bounds.x + digits_widths_summed,
                closest_info.lower_y + editor.config_info.font_height )
            else (closest_info.x + x_advance, closest_info.lower_y)
          in
          {
            closest_info with
            x = new_x;
            lower_y = new_y;
            upper_y = new_y + editor.config_info.font_height;
            closest_col;
            closest_rope;
            rope_pos = closest_info.rope_pos + 1;
          }
    in
    let lower_y =
      editor.bounds.y
      + (vertical_scroll_y_offset * editor.config_info.font_height)
    in
    let upper_y = lower_y + editor.config_info.font_height in
    let { closest_rope; _ } : closest_information =
      traverse_rope rope fold_fn_for_close_x
        {
          closest_rope = -1;
          closest_col = None;
          x = editor.bounds.x + digits_widths_summed;
          lower_y;
          upper_y;
          rope_pos = 0;
          closest_vertical_range;
        }
    in
    closest_rope

  (*
     The algorithm for finding the closest rope position in the rope data structure
     given an x and a y from mousedown event:

       1. Find the vertical range that the y value resides in by traversing the rope and
          building up x and y values according to the screen coordinates. Top left is the origin
          and far right is window_width (drawable size) and bottom most is window_height (drawable size).
       2. Find the horizontal x position, that is built from traversing the rope again given the vertical range,
          that is within the vertical range and is closest to the x value for the mousedown event.
   *)
  let find_closest_rope_pos_for_cursor_on_coords ~(editor : editor) ~x ~y
      ~digits_widths_summed =
    let window_width, _ = Sdl.sdl_gl_getdrawablesize () in
    let window_width_without_high_dpi, _ = Sdl.sdl_get_window_size Sdl.w in
    (* ratio is needed because the x,y coords given from MouseEvent is based on window without high dpi so scaling needs to happen *)
    let ratio = window_width / window_width_without_high_dpi in
    let x = x * ratio and y = y * ratio in
    let current_rope =
      List.nth editor.ropes (editor.current_rope_idx |> Option.get)
    in
    match current_rope with
    | File { rope; vertical_scroll_y_offset; _ } ->
        let rope = Option.get rope in
        let closest_vertical_range =
          find_closest_vertical_range ~editor ~rope ~digits_widths_summed
            ~vertical_scroll_y_offset ~y
        in
        let closest_rope =
          find_closest_horizontal_pos ~editor ~rope ~digits_widths_summed
            ~vertical_scroll_y_offset ~closest_vertical_range ~x
        in
        closest_rope
    | _ -> failwith "NOT FILE"

  let calc_new_xy ~(char : char) ~(editor : editor) ~x ~y ~digits_widths_summed
      =
    match char with
    | '\n' ->
        ( editor.bounds.x + digits_widths_summed,
          y + editor.config_info.font_height )
    | _ ->
        let _, gi =
          Array.find_opt
            (fun (c', _) -> c' = char)
            editor.config_info.glyph_info_with_char
          |> Option.get
        in
        let x_advance = FreeType.get_x_advance gi in
        if x + x_advance > editor.bounds.x + editor.bounds.width then
          ( editor.bounds.x + digits_widths_summed,
            y + editor.config_info.font_height )
        else (x + x_advance, y)

  type finding_cursor_pos_info = { x : int; y : int; rope_pos : int }

  let find_coords_for_cursor_pos ~(editor : editor) =
    let current_rope =
      List.nth editor.ropes (editor.current_rope_idx |> Option.get)
    in
    match current_rope with
    | File { cursor_pos; rope; vertical_scroll_y_offset; _ } ->
        let rope = Option.get rope in
        let num_lines = num_lines rope in
        let digits_widths_summed =
          get_digits_widths_summed ~num_lines ~editor
        in
        let fold_fn_for_finding_coords acc c =
          if acc.rope_pos != cursor_pos then
            let new_x, new_y =
              calc_new_xy ~editor ~x:acc.x ~y:acc.y ~digits_widths_summed
                ~char:c
            in
            { x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
          else acc
        in
        let res =
          traverse_rope rope fold_fn_for_finding_coords
            {
              x = editor.bounds.x + digits_widths_summed;
              y =
                editor.bounds.y
                + (vertical_scroll_y_offset * editor.config_info.font_height);
              rope_pos = 0;
            }
        in
        res
    | _ ->
        failwith
          "find_coords_for_cursor_pos not handled yet for other rope types"

  let find_closest_rope_pos_for_moving_cursor_in_vertical_range
      ~(editor : editor) ~cursor_x ~lower_y =
    let current_rope =
      List.nth editor.ropes (editor.current_rope_idx |> Option.get)
    in
    match current_rope with
    | File { vertical_scroll_y_offset; rope; _ } ->
        let rope = Option.get rope in
        let num_lines = num_lines rope in
        let digits_widths_summed =
          get_digits_widths_summed ~num_lines ~editor
        in
        find_closest_horizontal_pos ~editor ~rope ~x:cursor_x
          ~digits_widths_summed ~vertical_scroll_y_offset
          ~closest_vertical_range:
            (Some (lower_y, lower_y + editor.config_info.font_height))
    | _ -> failwith "supposed to be used for file modes"
end
