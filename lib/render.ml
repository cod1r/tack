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

  let draw_bmp_points w glyph_info pts offset =
    let floats =
      List.map
        (function
          | Sdl.Point (x, y) ->
              ( Sdl.PointF (Int.to_float x, Int.to_float y),
                Bytes.get glyph_info.FreeType.bitmap.buffer
                  ((y * glyph_info.FreeType.bitmap.pitch) + x) ))
        pts
    in
    List.iter
      (function
        | Sdl.PointF (x, y), byte ->
            let int_byte = Char.code byte in
            Sdl.sdl_set_render_draw_color w 0 0 0 int_byte;
            Sdl.sdl_render_draw_points_float w
              (* we are dividing by 3 here because of FT_RENDER_MODE_LCD *)
              [ PointF ((x /. 3.) +. fst offset, y +. snd offset) ])
      floats

  (* this function draws all of the glyphs in glyph_infos *)
  let draw_glyphs w glyph_infos biggest_horiBearingY =
    let rec get_points x y acc (bitmap : FreeType.freetype_bitmap) =
      let new_acc = Sdl.Point (x, y) :: acc in
      let width, rows = (bitmap.FreeType.width, bitmap.FreeType.rows) in
      if width > 0 && rows > 0 then
        match (x = width - 1, y = rows - 1) with
        | true, true -> new_acc
        | true, _ -> get_points 0 (succ y) new_acc bitmap
        | _ -> get_points (succ x) y new_acc bitmap
      else []
    in
    let rec get_xs glyphs x acc =
      match glyphs with
      | [] -> acc
      | (_, h) :: t -> get_xs t (x + fst h.FreeType.advance) (x :: acc)
    in
    let xs = get_xs glyph_infos 0 [] in
    let points_list =
      List.map
        (fun (_, glyph) -> (glyph, get_points 0 0 [] glyph.FreeType.bitmap))
        glyph_infos
    in
    List.iter2
      (fun (glyph, pl) x ->
        draw_bmp_points w glyph pl
          ( Int.to_float (x + glyph.FreeType.metrics.horiBearingX),
            Int.to_float biggest_horiBearingY
            -. Int.to_float glyph.FreeType.metrics.horiBearingY ))
      points_list xs

  let draw_letter_glyph w (x, y) g biggest_horiBearingY =
    let rec get_points x y acc (bitmap : FreeType.freetype_bitmap) =
      let new_acc = Sdl.Point (x, y) :: acc in
      let width, rows = (bitmap.FreeType.width, bitmap.FreeType.rows) in
      if width > 0 && rows > 0 then
        match (x = width - 1, y = rows - 1) with
        | true, true -> new_acc
        | true, _ -> get_points 0 (succ y) new_acc bitmap
        | _ -> get_points (succ x) y new_acc bitmap
      else []
    in
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
    let pts = get_points 0 0 [] g.FreeType.bitmap in
    draw_bmp_points w g pts
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
