open Freetype
open Rope
open Sdl

module Editor = struct
  type information_relating_to_config = {
    glyph_info_with_char : (char * FreeType.glyph_info) Array.t;
    ft_face : FreeType.ft_face;
    pixel_size : int;
    font_height : int;
  }

  type mode = Editing | FileSearch

  type editor = {
    rope : Rope.rope option;
    cursor_pos : int;
    holding_ctrl : bool;
    vertical_scroll_y_offset : int;
    highlight : (int * int) option;
    config_info : information_relating_to_config;
    search_rope : Rope.rope option;
    list_options_rope : Rope.rope option;
  }

  let config_has_been_modified_during_runtime () =
    let s = Unix.stat ".config.json" in
    Unix.time () -. s.st_mtime < 1.

  let read_config () =
    let config_str =
      In_channel.with_open_bin ".config.json" (fun ic ->
          In_channel.input_all ic)
    in
    Yojson.Safe.from_string config_str

  let recalculate_info_relating_to_config () =
    (let config = read_config () in
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
      : information_relating_to_config)

  type 'a rope_traversal_info = {
    acc_horizontal_x_pos : int;
    rope_pos : int;
    accumulation : 'a;
  }

  let rec traverse_rope (rope : Rope.rope)
      (handle_result : 'a rope_traversal_info -> char -> 'a rope_traversal_info)
      (result : 'a rope_traversal_info) =
    match rope with
    | Leaf l -> String.fold_left handle_result result l
    | Node { left; right; _ } ->
        let left_result = traverse_rope left handle_result result in
        traverse_rope right handle_result left_result

  let find_closest_rope_pos_for_cursor_on_coords (editor : editor)
      ((x, y) : int * int) =
    let window_width, _ = Sdl.sdl_gl_getdrawablesize () in
    let window_width_without_high_dpi, _ = Sdl.sdl_get_window_size Sdl.w in
    (* ratio is needed because the x,y coords given from MouseEvent is based on window without high dpi so scaling needs to happen *)
    let ratio = window_width / window_width_without_high_dpi in
    let compare_and_pick target value1 value2 =
      let diff1 = abs (target - value1) and diff2 = abs (target - value2) in
      if diff1 < diff2 then value1 else value2
    in
    let fold_fn
        {
          acc_horizontal_x_pos = acc_x_offset;
          rope_pos = rp;
          accumulation = acc_closest_x, acc_closest_y, acc_closest_rp;
        } c =
      let amt_window_widths = acc_x_offset / window_width in
      let lower_y_height =
        (amt_window_widths + editor.vertical_scroll_y_offset)
        * editor.config_info.font_height
      in
      let used_y =
        if
          lower_y_height <= y * ratio
          && y * ratio <= lower_y_height + editor.config_info.font_height
        then lower_y_height
        else acc_closest_y
      in
      if c = '\n' then
        let next_x_pos = (amt_window_widths + 1) * window_width in
        let line_x_pos = acc_x_offset mod window_width in
        let used_x =
          compare_and_pick (x * ratio) line_x_pos acc_closest_x
          |> min line_x_pos
        in
        {
          acc_horizontal_x_pos = next_x_pos;
          rope_pos = rp + 1;
          accumulation =
            ( used_x,
              used_y,
              if used_y = lower_y_height && used_x = line_x_pos then rp
              else acc_closest_rp );
        }
      else
        let glyph_info_found =
          Array.find_opt
            (fun (c', _) -> c' = c)
            editor.config_info.glyph_info_with_char
        in
        match glyph_info_found with
        | Some (_, gi) ->
            let x_advance = FreeType.get_x_advance gi in
            let amt_window_widths_plus_x_advance =
              (acc_x_offset + x_advance) / window_width
            in
            let processed_x_offset =
              if amt_window_widths_plus_x_advance > amt_window_widths then
                amt_window_widths_plus_x_advance * window_width
              else acc_x_offset
            in
            let calculated_x_offset = processed_x_offset mod window_width in
            let used_x =
              compare_and_pick (x * ratio) calculated_x_offset acc_closest_x
              |> min calculated_x_offset
            in
            let row =
              ((processed_x_offset / window_width)
              + editor.vertical_scroll_y_offset)
              * editor.config_info.font_height
            in
            {
              acc_horizontal_x_pos = processed_x_offset + x_advance;
              rope_pos = rp + 1;
              accumulation =
                ( used_x,
                  used_y,
                  if used_y = row && used_x = calculated_x_offset then rp
                  else acc_closest_rp );
            }
        | None -> failwith ("glyph_info not found for " ^ Char.escaped c)
    in
    let { accumulation = _, cy, crp; _ } =
      traverse_rope
        (editor.rope |> Option.get)
        fold_fn
        ({
           acc_horizontal_x_pos = 0;
           rope_pos = 0;
           accumulation = (Int.max_int, Int.max_int, Int.max_int);
         }
          : (int * int * int) rope_traversal_info)
    in
    Printf.printf "closest y: %d" cy;
    print_newline ();
    min crp (length (editor.rope |> Option.get))
end
