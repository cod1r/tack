open Freetype
open Sdl

module Render = struct
  let draw_cursor w (x, y) biggest_horiBearingY =
    Sdl.sdl_set_render_draw_color w 0l 0l 0l 255l;
    let r =
      Sdl.RectF { x; y; width = 3.; height = Int.to_float biggest_horiBearingY }
    in
    Sdl.sdl_renderer_draw_rect_float w r;
    Sdl.sdl_renderer_fill_rect_float w r

  let draw_bmp_points bigarray glyph_info offset =
    let length =
      glyph_info.FreeType.bitmap.width * glyph_info.FreeType.bitmap.rows * 3
    in
    let rec loop idx end_val =
      if idx >= end_val then ()
      else
        let i = idx / 3 in
        let x, y =
          ( i mod glyph_info.FreeType.bitmap.width,
            i / glyph_info.FreeType.bitmap.width )
        in
        let byte =
          Bytes.get glyph_info.FreeType.bitmap.buffer
            ((y * glyph_info.FreeType.bitmap.pitch) + x)
        in
        let int_byte = Char.code byte in
        (* we are dividing by 3 here because of FT_RENDER_MODE_LCD *)
        bigarray.{idx} <- (Int.to_float x /. 3.) +. fst offset;
        bigarray.{idx + 1} <- Int.to_float y +. snd offset;
        bigarray.{idx + 2} <- Int.to_float int_byte;
        loop (idx + 3) end_val
    in
    loop 0 length

  let draw_letter_glyph bigarray (x, y) g biggest_horiBearingY =
    let width_screen = 800 in
    let next_x = x + g.FreeType.metrics.horiBearingX in
    let used_x, used_y =
      if next_x >= width_screen then
        (g.FreeType.metrics.horiBearingX, y + biggest_horiBearingY)
      else (next_x, y)
    in
    let accx, accy =
      if next_x >= width_screen then
        (fst g.FreeType.advance, y + biggest_horiBearingY)
      else (x + fst g.FreeType.advance, y + snd g.FreeType.advance)
    in
    draw_bmp_points bigarray g
      ( Int.to_float used_x,
        Int.to_float used_y
        +. Int.to_float biggest_horiBearingY
        -. Int.to_float g.FreeType.metrics.horiBearingY );
    (accx, accy)

  let draw_rope bigarray rope biggest_horiBearingY =
    let rec draw_rope' bigarray rope offset =
      match rope with
      | Rope.Leaf l ->
          (* fold_right isn't tail_recursive and l is stored in reverse order *)
          List.fold_left
            (fun acc (c, g) -> draw_letter_glyph bigarray acc g biggest_horiBearingY)
            offset (List.rev l)
      | Rope.Node { left; right; _ } ->
          let left_offset = draw_rope' bigarray left offset in
          draw_rope' bigarray right left_offset
    in
    let _ = draw_rope' bigarray rope (0, 0) in
    ()

  let draw bigarray rope biggest_horiBearingY =
    (match rope with
    | Some r -> draw_rope bigarray r biggest_horiBearingY
    | None -> ());
end
