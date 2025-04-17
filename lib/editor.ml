open Freetype
open Rope
open Sdl

module Editor = struct
  type editor = { rope : Rope.rope option; cursor_pos : int }

  let glyph_info_with_char =
    Array.init
      (126 - 32 + 1)
      (fun i -> FreeType.get_ascii_char_glyph FreeType.face (i + 32))

  external get_proper_x_offset_value : int -> FreeType.glyph_info -> int -> int
    = "get_proper_x_offset_value" "get_proper_x_offset_value"

  let find_closest_rope_pos_to_coords (rope : Rope.rope) ((x, y) : int * int) =
    let window_width, _ = Sdl.sdl_gl_getdrawablesize () in
    let window_width_without_high_dpi, _ = Sdl.sdl_get_window_size Sdl.w in
    let ratio = window_width / window_width_without_high_dpi in
    let rec traverse_rope (rope : Rope.rope) (offset : int)
        (rope_position : int)
        ((closest_x, closest_y, closest_rope_pos) : int * int * int) =
      match rope with
      | Leaf l ->
          String.fold_right
            (fun c
                 ( acc_x_offset,
                   rp,
                   (acc_closest_x, acc_closest_y, acc_closest_rp) ) ->
              let glyph_info_found =
                Array.find_opt (fun (c', _) -> c' = c) glyph_info_with_char
              in
              match glyph_info_found with
              | Some (_, gi) ->
                  let x_advance = FreeType.get_x_advance gi in
                  let processed_x_offset =
                    get_proper_x_offset_value acc_x_offset gi window_width
                  in
                  let calculated_x = processed_x_offset mod window_width in
                  let calculated_y =
                    (processed_x_offset + x_advance)
                    / window_width * FreeType.font_height
                  in
                  let diff_x = abs ((x * ratio) - calculated_x)
                  and diff_y = abs ((y * ratio) - calculated_y)
                  and curr_diffx = abs ((x * ratio) - acc_closest_x)
                  and curr_diffy = abs ((y * ratio) - acc_closest_y) in
                  let new_closest =
                    if diff_x <= curr_diffx && diff_y <= curr_diffy then
                      (calculated_x, calculated_y, rp)
                    else (acc_closest_x, acc_closest_y, acc_closest_rp)
                  in
                  (processed_x_offset + x_advance, rp + 1, new_closest)
              | None -> failwith ("glyph_info not found for " ^ Char.escaped c))
            l
            (offset, rope_position, (closest_x, closest_y, closest_rope_pos))
      | Node { left; right; _ } ->
          let left_offset, lrp, left_closest =
            traverse_rope left offset rope_position
              (closest_x, closest_y, closest_rope_pos)
          in
          traverse_rope right left_offset lrp left_closest
    in
    let _, _, (_, _, crp) =
      traverse_rope rope 0 0 (Int.max_int, Int.max_int, Int.max_int)
    in
    crp
end
