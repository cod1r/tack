open Freetype
open Sdl
open Opengl

module Render = struct
  let vertex_shader =
    {|
  #version 120
  attribute vec3 point_vertex;
  varying float alpha;
  void main() {
    gl_Position = vec4(point_vertex.x, point_vertex.y, 0.0, 1.0);
    alpha = point_vertex.z;
  }
  |}

  let fragment_shader =
    {|
  #version 120
  varying float alpha;
  void main() {
    gl_FragColor = vec4(0.0, 0.0, 0.0, alpha);
  }
  |}

  let fragment =
    match gl_create_fragment_shader () with Ok f -> f | Error e -> failwith e

  let vertex =
    match gl_create_vertex_shader () with Ok v -> v | Error e -> failwith e

  let program =
    gl_shader_source fragment fragment_shader;
    gl_shader_source vertex vertex_shader;
    gl_compileshader fragment;
    if not (gl_get_shader_compile_status fragment) then
      failwith (gl_get_shader_info_log fragment);
    gl_compileshader vertex;
    if not (gl_get_shader_compile_status vertex) then
      failwith (gl_get_shader_info_log vertex);
    let p =
      match gl_createprogram () with Ok p -> p | Error e -> failwith e
    in
    gl_attach_shader p fragment;
    gl_attach_shader p vertex;
    p
  ;;

  Printf.printf "%d %d %d" program vertex fragment;
  print_newline ()

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
            (fun acc (c, g) ->
              draw_letter_glyph bigarray acc g biggest_horiBearingY)
            offset (List.rev l)
      | Rope.Node { left; right; _ } ->
          let left_offset = draw_rope' bigarray left offset in
          draw_rope' bigarray right left_offset
    in
    let _ = draw_rope' bigarray rope (0, 0) in
    ()

  let draw bigarray rope biggest_horiBearingY =
    match rope with
    | Some r -> draw_rope bigarray r biggest_horiBearingY
    | None -> ()
end
