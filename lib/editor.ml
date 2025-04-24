open Freetype
open Rope
open Sdl

module Editor = struct
  type editor = {
    rope : Rope.rope option;
    cursor_pos : int;
    holding_ctrl : bool;
    vertical_scroll_y_offset : int;
  }

  let glyph_info_with_char =
    Array.init
      (126 - 32 + 1)
      (fun i -> FreeType.get_ascii_char_glyph FreeType.face (i + 32))

  let rec traverse_rope (rope : Rope.rope) (handle_result : 'a -> char -> 'a)
      (result : 'a) =
    match rope with
    | Leaf l -> String.fold_left handle_result result l
    | Node { left; right; _ } ->
        let left_result = traverse_rope left handle_result result in
        traverse_rope right handle_result left_result

  let find_closest_rope_pos_for_cursor_on_mouse_down (editor : editor)
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
        (acc_x_offset, rp, (acc_closest_x, acc_closest_y, acc_closest_rp)) c =
      let amt_window_widths = acc_x_offset / window_width in
      let lower_y_height = amt_window_widths * FreeType.font_height in
      let used_y =
        if
          lower_y_height <= y * ratio
          && y * ratio <= lower_y_height + FreeType.font_height
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
        ( next_x_pos,
          rp + 1,
          ( used_x,
            used_y,
            if used_y = lower_y_height && used_x = line_x_pos then rp
            else acc_closest_rp ) )
      else
        let glyph_info_found =
          Array.find_opt (fun (c', _) -> c' = c) glyph_info_with_char
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
              processed_x_offset / window_width * FreeType.font_height
            in
            ( processed_x_offset + x_advance,
              rp + 1,
              ( used_x,
                used_y,
                if used_y = row && used_x = calculated_x_offset then rp
                else acc_closest_rp ) )
        | None -> failwith ("glyph_info not found for " ^ Char.escaped c)
    in
    let _, _, (_, _, crp) =
      traverse_rope
        (editor.rope |> Option.get)
        fold_fn
        ( editor.vertical_scroll_y_offset * window_width,
          0,
          (Int.max_int, Int.max_int, Int.max_int) )
    in
    min crp (length (editor.rope |> Option.get))
end
