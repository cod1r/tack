open Freetype
open Sdl

module Render = struct
  let draw_cursor w (x, y) =
    sdl_set_render_draw_color w 0 0 0 255;
    let r =
      RectF { x; y; width = 3.; height = Int.to_float biggest_horiBearingY }
    in
    sdl_renderer_draw_rect_float w r;
    sdl_renderer_fill_rect_float w r

  let draw_bmp_points w glyph_info pts offset =
    let floats =
      List.map
        (function
          | Point (x, y) ->
              ( PointF (Int.to_float x, Int.to_float y),
                Bytes.get glyph_info.FreeType.bitmap.buffer
                  ((y * glyph_info.FreeType.bitmap.pitch) + x) ))
        pts
    in
    List.iter
      (function
        | PointF (x, y), byte ->
            let int_byte = Char.code byte in
            sdl_set_render_draw_color w 0 0 0 int_byte;
            sdl_render_draw_points_float w
              (* we are dividing by 3 here because of FT_RENDER_MODE_LCD *)
              [ PointF ((x /. 3.) +. fst offset, y +. snd offset) ])
      floats

  let draw_glyphs w =
    let rec get_points x y acc (bitmap : FreeType.freetype_bitmap) =
      let new_acc = Point (x, y) :: acc in
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

  let draw_letter_glyph w letter (x, y) glyph_infos biggest_horiBearingY =
    let found_glyph =
      List.find_opt (fun (c, _) -> Char.chr c = letter) glyph_infos
    in
    match found_glyph with
    | Some (_, g) ->
        let rec get_points x y acc (bitmap : FreeType.freetype_bitmap) =
          let new_acc = Point (x, y) :: acc in
          let width, rows = (bitmap.FreeType.width, bitmap.FreeType.rows) in
          if width > 0 && rows > 0 then
            match (x = width - 1, y = rows - 1) with
            | true, true -> new_acc
            | true, _ -> get_points 0 (succ y) new_acc bitmap
            | _ -> get_points (succ x) y new_acc bitmap
          else []
        in
        let pts = get_points 0 0 [] g.FreeType.bitmap in
        draw_bmp_points w g pts
          ( Int.to_float (x + g.FreeType.metrics.horiBearingX),
            Int.to_float y
            +. Int.to_float biggest_horiBearingY
            -. Int.to_float g.FreeType.metrics.horiBearingY );
        sdl_render_present w;
        (x + fst g.FreeType.advance, y + snd g.FreeType.advance)
    | None -> (x, y)
end
