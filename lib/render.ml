open Freetype
open Sdl

module Render = struct
  let draw_cursor w (x, y) biggest_horiBearingY =
    Sdl.sdl_set_render_draw_color w 0 0 0 255;
    let r =
      Sdl.RectF { x; y; width = 3.; height = Int.to_float biggest_horiBearingY }
    in
    Sdl.sdl_renderer_draw_rect_float w r;
    Sdl.sdl_renderer_fill_rect_float w r

  let draw_bmp_points w glyph_info offset =
    let indices =
      List.init
        (glyph_info.FreeType.bitmap.width * glyph_info.FreeType.bitmap.rows)
        (fun i -> i)
    in
    List.iter
      (function
        | i ->
            let x, y =
              ( i mod glyph_info.FreeType.bitmap.width,
                i / glyph_info.FreeType.bitmap.width )
            in
            let byte =
              Bytes.get glyph_info.FreeType.bitmap.buffer
                ((y * glyph_info.FreeType.bitmap.pitch) + x)
            in
            let int_byte = Char.code byte in
            Sdl.sdl_set_render_draw_color w 0 0 0 int_byte;
            Sdl.sdl_render_draw_point_f w
              (* we are dividing by 3 here because of FT_RENDER_MODE_LCD *)
              ((Int.to_float x /. 3.) +. fst offset)
              (Int.to_float y +. snd offset))
      indices

  let draw_letter_glyph w (x, y) g biggest_horiBearingY =
    let width_screen, _ = Sdl.sdl_get_renderer_size w in
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
    draw_bmp_points w g
      ( Int.to_float used_x,
        Int.to_float used_y
        +. Int.to_float biggest_horiBearingY
        -. Int.to_float g.FreeType.metrics.horiBearingY );
    (accx, accy)

  let draw_rope w rope biggest_horiBearingY =
    let rec draw_rope' w rope offset =
      match rope with
      | Rope.Leaf l ->
          List.fold_right
            (fun (c, g) acc -> draw_letter_glyph w acc g biggest_horiBearingY)
            l offset
      | Rope.Node { left; right; _ } ->
          let left_offset = draw_rope' w left offset in
          draw_rope' w right left_offset
    in
    let _ = draw_rope' w rope (0, 0) in
    ()

  let draw w rope biggest_horiBearingY =
    Sdl.sdl_set_render_draw_color w 255 255 255 255;
    Sdl.sdl_render_clear w;
    (match rope with
    | Some r -> draw_rope w r biggest_horiBearingY
    | None -> ());
    Sdl.sdl_render_present w
end
