open Freetype
open Rope
open Sdl

let _LINE_NUMBER_RIGHT_PADDING = 20

module Editor = struct
  type information_relating_to_config = {
    glyph_info_with_char : (char * FreeType.glyph_info_) Array.t;
    ft_face : FreeType.ft_face;
    pixel_size : int;
    font_height : int;
    descender : int;
    font_glyph_texture_atlas_info : Ui.text_texture_atlas_info;
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

  type rope_traversal_info_ = {
    x : int;
    y : int;
    rope_pos : int;
    line_num : int;
    line_number_placements : (int * int) list;
  }

  type closest_information = {
    closest_col : int option;
    x : int;
    lower_y : int;
    upper_y : int;
    closest_vertical_range : (int * int) option;
    closest_rope : int;
    rope_pos : int;
  }

  type rope_traversal_info = { x : int; y : int; rope_pos : int }

  type _ traverse_info =
    | Rope_Traversal_Info :
        rope_traversal_info_
        -> rope_traversal_info_ traverse_info
    | Num_Lines : int -> int traverse_info
    | Finding_Cursor : closest_information -> closest_information traverse_info
    | Finding_Coords_Cursor :
        rope_traversal_info
        -> rope_traversal_info traverse_info

  let open_file file_name =
    Printf.printf "Trying to open %s" file_name;
    print_newline ();
    In_channel.with_open_bin file_name (fun ic -> In_channel.input_all ic)
    |> Rope.of_string

  let config_has_been_modified_during_runtime () =
    let s = Unix.stat ".config.json" in
    Unix.time () -. s.st_mtime < 1.

  let recalculate_info_relating_to_config () : information_relating_to_config =
    let config = Config.read_config () in
    let font_pixel_size =
      Yojson.Safe.Util.member "font_size" config |> Yojson.Safe.Util.to_int
    and font_path =
      Yojson.Safe.Util.member "font_path" config |> Yojson.Safe.Util.to_string
    in
    let face = FreeType.freetype_get_face font_path FreeType.library in
    let glyph_info_with_char =
      Array.init
        (126 - 32 + 1)
        (fun i ->
          FreeType.get_ascii_char_glyph_info_ face (i + 32) font_pixel_size)
    in
    (* need to call font_height after set_pixel_sizes *)
    let font_height = FreeType.get_font_height face in
    let descender = FreeType.get_descender face in
    let ascender = FreeType.get_ascender face in
    let widths_summed =
      Array.fold_left
        (fun acc (_, gi) -> acc + gi.FreeType.width)
        0 glyph_info_with_char
    in
    let global_font_height = ascender - descender in
    let bytes_texture_atlas =
      Bytes.create (widths_summed * global_font_height)
    in
    (*
       the font glyph texture atlas is all of the glyphs that is loaded
       concatenated into a large bytes array

       widths of glyphs summed * font height
       ex:
         ABCDEF...
     *)
    let current_width = ref 0 in
    for glyph_info_index = 0 to Array.length glyph_info_with_char - 1 do
      let _, glyph_info = glyph_info_with_char.(glyph_info_index) in
      for row = 0 to glyph_info.rows - 1 do
        let slice =
          Bytes.sub glyph_info.bytes (row * glyph_info.width) glyph_info.width
        in
        Bytes.blit slice 0 bytes_texture_atlas
          (!current_width + (widths_summed * row))
          glyph_info.width
      done;
      current_width := !current_width + glyph_info.width
    done;
    let font_glyph_texture_atlas_info : Ui.text_texture_atlas_info =
      {
        width = widths_summed;
        height = global_font_height;
        bytes = bytes_texture_atlas;
      }
    in
    FreeType.freetype_done_face face;
    {
      glyph_info_with_char;
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

  let rec traverse_rope : type p.
      _ -> (p traverse_info -> char -> p traverse_info) -> p traverse_info -> p
      =
   fun (rope : Rope.rope)
       (handle_result : p traverse_info -> char -> p traverse_info)
       (result : p traverse_info) ->
    match rope with
    | Leaf l -> (
        match result with
        | Rope_Traversal_Info r ->
            let acc = ref r in
            let len = String.length l in
            for i = 0 to len - 1 do
              let (Rope_Traversal_Info temp) =
                handle_result (Rope_Traversal_Info !acc) l.[i]
              in
              acc := temp
            done;
            !acc
        | Finding_Coords_Cursor r ->
            let acc = ref r in
            let len = String.length l in
            for i = 0 to len - 1 do
              let (Finding_Coords_Cursor temp) =
                handle_result (Finding_Coords_Cursor !acc) l.[i]
              in
              acc := temp
            done;
            !acc
        | Finding_Cursor r ->
            let acc = ref r in
            let len = String.length l in
            for i = 0 to len - 1 do
              let (Finding_Cursor temp) =
                handle_result (Finding_Cursor !acc) l.[i]
              in
              acc := temp
            done;
            !acc
        | Num_Lines r ->
            let acc = ref r in
            let len = String.length l in
            for i = 0 to len - 1 do
              let (Num_Lines temp) = handle_result (Num_Lines !acc) l.[i] in
              acc := temp
            done;
            !acc)
    | Node { left; right; _ } ->
        let left_result = traverse_rope left handle_result result in
        let right_result =
          traverse_rope right handle_result
            (match result with
            | Rope_Traversal_Info _ -> Rope_Traversal_Info left_result
            | Finding_Coords_Cursor _ -> Finding_Coords_Cursor left_result
            | Finding_Cursor _ -> Finding_Cursor left_result
            | Num_Lines _ -> Num_Lines left_result)
        in
        right_result

  let num_lines (rope : Rope.rope) =
    let fold_fn (acc : int traverse_info) ch =
      if ch = '\n' then
        let (Num_Lines l) = acc in
        Num_Lines (l + 1)
      else acc
    in
    let accumulation = traverse_rope rope fold_fn (Num_Lines 0) in
    accumulation

  let get_digits_widths_summed ~(num_lines : int) ~(editor : editor) =
    let glyph_list = Array.to_list editor.config_info.glyph_info_with_char in
    let str = string_of_int num_lines in
    let len = String.length str in
    let glyphs = ref [] in
    for str_i = 0 to len - 1 do
      let glyph = List.find (fun (c', _) -> c' = str.[str_i]) glyph_list in
      glyphs := glyph :: !glyphs
    done;
    let digit_widths_summed =
      List.fold_left (fun acc (_, gi) -> acc + gi.FreeType.x_advance) 0 !glyphs
    in
    digit_widths_summed + _LINE_NUMBER_RIGHT_PADDING

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
    let fold_fn_for_vertical_range
        (closest_info : closest_information traverse_info) c =
      let (Finding_Cursor closest_info) = closest_info in
      match c with
      | '\n' ->
          Finding_Cursor
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
          let x_advance = gi.x_advance in
          let new_x, new_y =
            if
              closest_info.x + x_advance > editor.bounds.x + editor.bounds.width
            then
              ( editor.bounds.x + digits_widths_summed,
                closest_info.lower_y + editor.config_info.font_height )
            else (closest_info.x + x_advance, closest_info.lower_y)
          in
          Finding_Cursor
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
        (Finding_Cursor
           {
             lower_y;
             upper_y;
             closest_col = None;
             x = editor.bounds.x + digits_widths_summed;
             closest_rope = 0;
             rope_pos = 0;
             closest_vertical_range = None;
           })
    in
    closest_vertical_range

  let find_closest_horizontal_pos ~(editor : editor) ~rope ~x
      ~digits_widths_summed ~vertical_scroll_y_offset ~closest_vertical_range =
    let fold_fn_for_close_x (closest_info : closest_information traverse_info) c
        =
      let (Finding_Cursor closest_info) = closest_info in
      match c with
      | '\n' ->
          let closest_col, closest_rope =
            get_pair_col_and_rope_pos ~closest_info ~x
          in
          Finding_Cursor
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
          let x_advance = gi.x_advance in
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
          Finding_Cursor
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
        (Finding_Cursor
           {
             closest_rope = -1;
             closest_col = None;
             x = editor.bounds.x + digits_widths_summed;
             lower_y;
             upper_y;
             rope_pos = 0;
             closest_vertical_range;
           })
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
        if closest_rope = -1 then length rope else closest_rope
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
        let x_advance = gi.x_advance in
        if x + x_advance > editor.bounds.x + editor.bounds.width then
          ( editor.bounds.x + digits_widths_summed,
            y + editor.config_info.font_height )
        else (x + x_advance, y)

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
        let fold_fn_for_finding_coords (acc : rope_traversal_info traverse_info)
            c =
          let (Finding_Coords_Cursor acc) = acc in
          if acc.rope_pos != cursor_pos then
            let new_x, new_y =
              calc_new_xy ~editor ~x:acc.x ~y:acc.y ~digits_widths_summed
                ~char:c
            in
            Finding_Coords_Cursor
              { x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
          else Finding_Coords_Cursor acc
        in
        traverse_rope rope fold_fn_for_finding_coords
          (Finding_Coords_Cursor
             {
               x = editor.bounds.x + digits_widths_summed;
               y =
                 editor.bounds.y
                 + (vertical_scroll_y_offset * editor.config_info.font_height);
               rope_pos = 0;
             })
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
        let hor_pos =
          find_closest_horizontal_pos ~editor ~rope ~x:cursor_x
            ~digits_widths_summed ~vertical_scroll_y_offset
            ~closest_vertical_range:
              (Some (lower_y, lower_y + editor.config_info.font_height))
        in
        hor_pos
    | _ -> failwith "supposed to be used for file modes"
end
