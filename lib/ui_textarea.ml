open Freetype
open Sdl
open Opengl

let _LINE_NUMBER_RIGHT_PADDING = 20

module Editor = struct
  type file = {
    rope : Rope.rope option;
    cursor_pos : int;
    file_name : string;
    scroll_y_offset : int;
    last_modification_time : float;
    highlight : (int * int) option;
  }

  type editor = {
    ropes : file list;
    holding_ctrl : bool;
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

  let default_editor : editor =
    let width, height = Sdl.sdl_gl_getdrawablesize () in
    {
      ropes = [];
      holding_ctrl = false;
      current_rope_idx = None;
      bounds = { width; height; x = 0; y = 0 };
    }
end

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

let rec traverse_rope : type p.
    _ -> (p traverse_info -> char -> p traverse_info) -> p traverse_info -> p =
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

let get_digits_widths_summed ~(num_lines : int) ~(font_info : Ui.font_info) =
  let glyph_list = Array.to_list font_info.glyph_info_with_char in
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

let find_closest_vertical_range ~(bbox : Ui.bounding_box)
    ~(font_info : Ui.font_info) ~rope ~digits_widths_summed ~y ~scroll_y_offset
    =
  let fold_fn_for_vertical_range
      (closest_info : closest_information traverse_info) c =
    let (Finding_Cursor closest_info) = closest_info in
    match c with
    | '\n' ->
        Finding_Cursor
          {
            closest_info with
            lower_y = closest_info.lower_y + font_info.font_height;
            upper_y = closest_info.upper_y + font_info.font_height;
            x = bbox.x + digits_widths_summed;
            closest_vertical_range =
              (if y >= closest_info.lower_y && y <= closest_info.upper_y then
                 Some (closest_info.lower_y, closest_info.upper_y)
               else closest_info.closest_vertical_range);
          }
    | _ ->
        let _, gi =
          Array.find_opt (fun (c', _) -> c' = c) font_info.glyph_info_with_char
          |> Option.get
        in
        let x_advance = gi.x_advance in
        let new_x, new_y =
          if closest_info.x + x_advance > bbox.x + bbox.width then
            ( bbox.x + digits_widths_summed,
              closest_info.lower_y + font_info.font_height )
          else (closest_info.x + x_advance, closest_info.lower_y)
        in
        Finding_Cursor
          {
            closest_info with
            x = new_x;
            lower_y = new_y;
            upper_y = new_y + font_info.font_height;
            closest_vertical_range =
              (if y >= closest_info.lower_y && y <= closest_info.upper_y then
                 Some (closest_info.lower_y, closest_info.upper_y)
               else closest_info.closest_vertical_range);
          }
  in
  let lower_y = bbox.y + (scroll_y_offset * font_info.font_height) in
  let upper_y = lower_y + font_info.font_height in
  let { closest_vertical_range; _ } : closest_information =
    traverse_rope rope fold_fn_for_vertical_range
      (Finding_Cursor
         {
           lower_y;
           upper_y;
           closest_col = None;
           x = bbox.x + digits_widths_summed;
           closest_rope = 0;
           rope_pos = 0;
           closest_vertical_range = None;
         })
  in
  closest_vertical_range

let find_closest_horizontal_pos ~(bbox : Ui.bounding_box)
    ~(font_info : Ui.font_info) ~rope ~x ~digits_widths_summed ~scroll_y_offset
    ~closest_vertical_range =
  let fold_fn_for_close_x (closest_info : closest_information traverse_info) c =
    let (Finding_Cursor closest_info) = closest_info in
    match c with
    | '\n' ->
        let closest_col, closest_rope =
          get_pair_col_and_rope_pos ~closest_info ~x
        in
        Finding_Cursor
          {
            closest_info with
            lower_y = closest_info.lower_y + font_info.font_height;
            upper_y = closest_info.upper_y + font_info.font_height;
            x = bbox.x + digits_widths_summed;
            rope_pos = closest_info.rope_pos + 1;
            closest_col;
            closest_rope;
          }
    | _ ->
        let _, gi =
          Array.find_opt (fun (c', _) -> c' = c) font_info.glyph_info_with_char
          |> Option.get
        in
        let x_advance = gi.x_advance in
        let closest_col, closest_rope =
          get_pair_col_and_rope_pos ~closest_info ~x
        in
        let new_x, new_y =
          if closest_info.x + x_advance > bbox.x + bbox.width then
            ( bbox.x + digits_widths_summed,
              closest_info.lower_y + font_info.font_height )
          else (closest_info.x + x_advance, closest_info.lower_y)
        in
        Finding_Cursor
          {
            closest_info with
            x = new_x;
            lower_y = new_y;
            upper_y = new_y + font_info.font_height;
            closest_col;
            closest_rope;
            rope_pos = closest_info.rope_pos + 1;
          }
  in
  let lower_y = bbox.y + (scroll_y_offset * font_info.font_height) in
  let upper_y = lower_y + font_info.font_height in
  let { closest_rope; _ } : closest_information =
    traverse_rope rope fold_fn_for_close_x
      (Finding_Cursor
         {
           closest_rope = -1;
           closest_col = None;
           x = bbox.x + digits_widths_summed;
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
let find_closest_rope_pos_for_cursor_on_coords ~(bbox : Ui.bounding_box)
    ~(font_info : Ui.font_info) ~x ~y ~digits_widths_summed ~rope
    ~scroll_y_offset =
  let window_width, _ = Sdl.sdl_gl_getdrawablesize () in
  let window_width_without_high_dpi, _ = Sdl.sdl_get_window_size Sdl.w in
  (* ratio is needed because the x,y coords given from MouseEvent is based on window without high dpi so scaling needs to happen *)
  let ratio = window_width / window_width_without_high_dpi in
  let x = x * ratio and y = y * ratio in
  let closest_vertical_range =
    find_closest_vertical_range ~bbox ~font_info ~rope ~digits_widths_summed
      ~scroll_y_offset ~y
  in
  let closest_rope =
    find_closest_horizontal_pos ~bbox ~font_info ~rope ~digits_widths_summed
      ~scroll_y_offset ~closest_vertical_range ~x
  in
  if closest_rope = -1 then Rope.length rope else closest_rope

let calc_new_xy ~(bbox : Ui.bounding_box) ~(char : char)
    ~(font_info : Ui.font_info) ~x ~y ~digits_widths_summed =
  match char with
  | '\n' -> (bbox.x + digits_widths_summed, y + font_info.font_height)
  | _ ->
      let _, gi =
        Array.find_opt (fun (c', _) -> c' = char) font_info.glyph_info_with_char
        |> Option.get
      in
      let x_advance = gi.x_advance in
      if x + x_advance > bbox.x + bbox.width then
        (bbox.x + digits_widths_summed, y + font_info.font_height)
      else (x + x_advance, y)

let find_coords_for_cursor_pos ~(font_info : Ui.font_info)
    ~(bbox : Ui.bounding_box) ~rope ~cursor_pos ~scroll_y_offset =
  let num_lines = num_lines rope in
  let digits_widths_summed = get_digits_widths_summed ~num_lines ~font_info in
  let fold_fn_for_finding_coords (acc : rope_traversal_info traverse_info) c =
    let (Finding_Coords_Cursor acc) = acc in
    if acc.rope_pos != cursor_pos then
      let new_x, new_y =
        calc_new_xy ~bbox ~font_info ~x:acc.x ~y:acc.y ~digits_widths_summed
          ~char:c
      in
      Finding_Coords_Cursor
        { x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
    else Finding_Coords_Cursor acc
  in
  traverse_rope rope fold_fn_for_finding_coords
    (Finding_Coords_Cursor
       {
         x = bbox.x + digits_widths_summed;
         y = bbox.y + (scroll_y_offset * font_info.font_height);
         rope_pos = 0;
       })

let find_closest_rope_pos_for_moving_cursor_in_vertical_range
    ~(font_info : Ui.font_info) ~cursor_x ~lower_y ~rope ~scroll_y_offset =
  let num_lines = num_lines rope in
  let digits_widths_summed = get_digits_widths_summed ~num_lines ~font_info in
  let hor_pos =
    find_closest_horizontal_pos ~font_info ~rope ~x:cursor_x
      ~digits_widths_summed ~scroll_y_offset
      ~closest_vertical_range:(Some (lower_y, lower_y + font_info.font_height))
  in
  hor_pos

let _EACH_POINT_FLOAT_AMOUNT = 6
let _EACH_POINT_FLOAT_AMOUNT_TEXT = 7

let text_vertex_shader =
  {|
  #version 120

  attribute vec2 vertex;
  attribute vec3 color;
  attribute vec2 tex_coord;

  varying vec3 color_frag;
  varying vec2 tex_coord_frag;

  void main() {
    color_frag = color;
    tex_coord_frag = tex_coord;
    gl_Position = vec4(vertex.x, vertex.y, 0.0, 1.0);
  }
  |}

let text_fragment_shader =
  {|
  #version 120

  varying vec2 tex_coord_frag;
  varying vec3 color_frag;

  uniform sampler2D sampler;

  void main() {
    gl_FragColor = vec4(color_frag.r, color_frag.g, color_frag.b, texture2D(sampler, tex_coord_frag).a);
  }
  |}

let generic_vertex_shader =
  {|
  #version 120

  attribute vec2 point_vertex;
  attribute vec4 color_attrib;

  varying vec4 color;

  void main() {
    gl_Position = vec4(point_vertex.x, point_vertex.y, 0.0, 1.0);
    color = color_attrib;
  }
  |}

let generic_fragment_shader =
  {|
  #version 120
  varying vec4 color;
  void main() {
    gl_FragColor = vec4(color.r, color.g, color.b, color.a);
  }
  |}

let compile_shaders_and_return_program ~vertex_id ~fragment_id ~vertex_src
    ~fragment_src =
  gl_shader_source fragment_id fragment_src;
  gl_shader_source vertex_id vertex_src;
  gl_compileshader fragment_id;
  if not (gl_get_shader_compile_status fragment_id) then
    failwith (gl_get_shader_info_log fragment_id);
  gl_compileshader vertex_id;
  if not (gl_get_shader_compile_status vertex_id) then
    failwith (gl_get_shader_info_log vertex_id);
  let p = match gl_createprogram () with Ok p -> p | Error e -> failwith e in
  gl_attach_shader p fragment_id;
  gl_attach_shader p vertex_id;
  gl_linkprogram p;
  p

type render_buffer_wrapper = {
  buffer : Opengl.render_buffer;
  mutable length : int;
}

type font_info_type = {
  glyph_info_with_char : (char * FreeType.glyph_info_) Array.t;
  font_glyph_texture_atlas_info : Ui.text_texture_atlas_info;
}

let get_tex_coords ~(config : font_info_type) ~(glyph : char)
    ~(glyph_info : FreeType.glyph_info_) =
  let _, starting_x =
    Array.fold_left
      (fun (found, acc) (c, gi) ->
        if c = glyph || found then (true, acc)
        else (false, acc + gi.FreeType.width))
      (false, 0) config.glyph_info_with_char
  in
  let starting_x, ending_x = (starting_x, starting_x + glyph_info.width) in
  let width_float = Float.of_int config.font_glyph_texture_atlas_info.width in
  let height_float = Float.of_int config.font_glyph_texture_atlas_info.height in
  let left = Float.of_int starting_x /. width_float in
  let right = Float.of_int ending_x /. width_float in
  (left, right, 0., Float.of_int glyph_info.rows /. height_float)

let write_to_text_buffer ~(render_buf_container : render_buffer_wrapper)
    ~(glyph_info : FreeType.glyph_info_) ~x ~y ~(glyph : char)
    ~(font_info : Ui.font_info) =
  let window_width, window_height = Sdl.sdl_get_window_size Sdl.w in
  let window_width_gl, window_height_gl = Sdl.sdl_gl_getdrawablesize () in
  let width_ratio = Float.of_int (window_width_gl / window_width) in
  let height_ratio = Float.of_int (window_height_gl / window_height) in
  let x_scaled, y_scaled =
    List.map (fun v -> Float.of_int v) [ x; y ] |> function
    | first :: second :: _ ->
        ( first /. Float.of_int window_width *. width_ratio,
          second /. Float.of_int window_height *. height_ratio )
    | _ -> failwith "failed to match list"
  in
  let width_scaled =
    Float.of_int glyph_info.width /. Float.of_int window_width *. width_ratio
  and height_scaled =
    Float.of_int glyph_info.rows /. Float.of_int window_height *. height_ratio
  in
  let left, right, top, bottom =
    get_tex_coords
      ~config:
        {
          glyph_info_with_char = font_info.glyph_info_with_char;
          font_glyph_texture_atlas_info = font_info.font_texture_atlas;
        }
      ~glyph ~glyph_info
  and horiBearing_Y_Scaled =
    Float.of_int glyph_info.horiBearingY
    /. Float.of_int window_height *. height_ratio
  and horiBearing_X_Scaled =
    Float.of_int glyph_info.horiBearingX
    /. Float.of_int window_width *. width_ratio
  in
  (*
     layout of the values list is:
       vertex x
       vertex y
       r
       g
       b
       texel coord x
       texel coord y

      that is repeated for the 4 points of the quad
   *)
  let values =
    [|
      x_scaled +. horiBearing_X_Scaled -. 1.;
      -.(y_scaled +. height_scaled) +. horiBearing_Y_Scaled +. 1.;
      0.;
      0.;
      0.;
      left;
      bottom;
      x_scaled +. horiBearing_X_Scaled -. 1.;
      -.y_scaled +. horiBearing_Y_Scaled +. 1.;
      0.;
      0.;
      0.;
      left;
      top;
      x_scaled +. width_scaled +. horiBearing_X_Scaled -. 1.;
      -.y_scaled +. horiBearing_Y_Scaled +. 1.;
      0.;
      0.;
      0.;
      right;
      top;
      x_scaled +. width_scaled +. horiBearing_X_Scaled -. 1.;
      -.(y_scaled +. height_scaled) +. horiBearing_Y_Scaled +. 1.;
      0.;
      0.;
      0.;
      right;
      bottom;
    |]
  in
  let start = render_buf_container.length in
  Array.iteri
    (fun idx v -> render_buf_container.buffer.{idx + start} <- v)
    values;
  render_buf_container.length <- start + Array.length values

let write_to_cursor_buffer ~(cursor_buffer : render_buffer_wrapper) ~x ~y
    ~window_width ~window_height ~font_height =
  let points =
    [ (x, y + font_height); (x, y); (x + 10, y); (x + 10, y + font_height) ]
  in
  let values = Array.init 24 (fun _ -> 0.) in
  List.iteri
    (fun idx (x, y) ->
      let x = Float.of_int x /. Float.of_int (window_width / 2) in
      let x = x -. 1. in
      let y = Float.of_int y /. Float.of_int (window_height / 2) in
      let y = -.y +. 1. in
      let start = idx * 6 in
      values.(start) <- x;
      values.(start + 1) <- y;
      values.(start + 2) <- 0.;
      values.(start + 3) <- 0.;
      values.(start + 4) <- 0.;
      values.(start + 5) <- 1.)
    points;
  Array.iteri (fun idx v -> cursor_buffer.buffer.{idx} <- v) values;
  cursor_buffer.length <- Array.length values

let draw_highlight ~(bbox : Ui.bounding_box) ~(font_info : Ui.font_info)
    ~(r : Rope.rope) ~highlight ~scroll_y_offset ~window_width ~window_height
    ~highlight_buffer ~digits_widths_summed =
  match highlight with
  | Some (highlight_start, highlight_end) ->
      let fold_fn_for_draw_highlight (acc : rope_traversal_info_ traverse_info)
          c =
        let (Rope_Traversal_Info acc) = acc in
        match c with
        | '\n' ->
            Rope_Traversal_Info
              { acc with y = acc.y + 1; rope_pos = acc.rope_pos + 1 }
        | _ ->
            let _, glyph_info_found =
              Array.find_opt
                (fun (c', _) -> c' = c)
                font_info.glyph_info_with_char
              |> Option.get
            in
            let x_advance = glyph_info_found.x_advance in
            let next_y = acc.y + font_info.font_height in
            let new_x, new_y =
              if acc.x + x_advance > bbox.x + bbox.width then (bbox.x, next_y)
              else (acc.x + x_advance, acc.y)
            in
            (if acc.rope_pos >= highlight_start && acc.rope_pos < highlight_end
             then
               let points =
                 [
                   (acc.x + digits_widths_summed, next_y);
                   (acc.x + digits_widths_summed, acc.y);
                   (acc.x + x_advance + digits_widths_summed, acc.y);
                   (acc.x + x_advance + digits_widths_summed, next_y);
                 ]
               in
               List.iter
                 (fun (x, y) ->
                   failwith "TODO: write highlight info into ui buffer")
                 points);
            Rope_Traversal_Info
              { acc with x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
      in
      let _ =
        traverse_rope r fold_fn_for_draw_highlight
          (Rope_Traversal_Info
             {
               x = bbox.x;
               y = bbox.y;
               rope_pos = 0;
               line_number_placements = [];
               line_num = 0;
             })
      in
      ()
  | None -> ()

let draw_line_numbers ~(bbox : Ui.bounding_box) ~(font_info : Ui.font_info)
    ~scroll_y_offset ~window_width ~window_height ~text_buffer
    ~line_number_placements =
  List.iter
    (fun (line_num, start_y) ->
      let digits = string_of_int line_num in
      let glyph_infos =
        String.fold_right
          (fun c' acc ->
            (Array.find_opt
               (fun (c'', _) -> c'' = c')
               font_info.glyph_info_with_char
            |> Option.get)
            :: acc)
          digits []
      in
      let y_pos = (scroll_y_offset * font_info.font_height) + start_y in
      if y_pos >= bbox.y && y_pos <= bbox.y + bbox.height then
        let curr_x_offset = ref 0 in
        List.iter
          (fun (c', gi) ->
            write_to_text_buffer ~font_info ~render_buf_container:text_buffer
              ~glyph_info:gi ~x:!curr_x_offset
              ~y:(y_pos + font_info.descender)
              ~glyph:c';
            curr_x_offset := !curr_x_offset + gi.FreeType.x_advance)
          glyph_infos)
    line_number_placements

let draw_cursor ~(font_info : Ui.font_info) ~(bbox : Ui.bounding_box)
    ~(r : Rope.rope) ~cursor_pos ~(cursor_buffer : render_buffer_wrapper)
    ~scroll_y_offset ~window_width ~window_height ~digits_widths_summed =
  let fold_fn_draw_cursor (acc : rope_traversal_info_ traverse_info) c =
    let (Rope_Traversal_Info acc) = acc in
    let y_pos = acc.y in
    if
      acc.rope_pos = cursor_pos && y_pos >= bbox.y && y_pos >= bbox.y
      && y_pos <= bbox.y + bbox.height
      && acc.x >= bbox.x
      && acc.x <= bbox.x + bbox.width
    then
      write_to_cursor_buffer ~cursor_buffer ~x:acc.x ~y:acc.y ~window_width
        ~window_height ~font_height:font_info.font_height;
    match c with
    | '\n' ->
        Rope_Traversal_Info
          {
            acc with
            x = bbox.x + digits_widths_summed;
            y = acc.y + font_info.font_height;
            rope_pos = acc.rope_pos + 1;
          }
    | _ ->
        let _, glyph_info =
          Array.find_opt (fun (c', _) -> c' = c) font_info.glyph_info_with_char
          |> Option.get
        in
        let x_advance = glyph_info.x_advance in
        let wraps = acc.x + x_advance > bbox.x + bbox.width in
        Rope_Traversal_Info
          {
            acc with
            x =
              (if wraps then bbox.x + digits_widths_summed + x_advance
               else acc.x + x_advance);
            y = (if wraps then acc.y + font_info.font_height else acc.y);
            rope_pos = acc.rope_pos + 1;
          }
  in
  let res =
    traverse_rope r fold_fn_draw_cursor
      (Rope_Traversal_Info
         {
           line_number_placements = [];
           x = bbox.x + digits_widths_summed;
           y = bbox.y + (scroll_y_offset * font_info.font_height);
           rope_pos = 0;
           line_num = 0;
         })
  in
  if res.rope_pos = cursor_pos then
    write_to_cursor_buffer ~cursor_buffer ~x:res.x ~y:res.y ~window_width
      ~window_height ~font_height:font_info.font_height

let draw_text ~(font_info : Ui.font_info) ~(bbox : Ui.bounding_box)
    ~(rope : Rope.rope) ~text_buffer ~window_width ~window_height
    ~digits_widths_summed ~scroll_y_offset =
  let fold_fn_for_drawing_text (acc : rope_traversal_info_ traverse_info) c =
    let (Rope_Traversal_Info acc) = acc in
    if c = '\n' then
      Rope_Traversal_Info
        {
          x = digits_widths_summed;
          y = acc.y + font_info.font_height;
          rope_pos = acc.rope_pos + 1;
          line_num = acc.line_num + 1;
          line_number_placements =
            (acc.line_num + 1, acc.y + font_info.font_height)
            :: acc.line_number_placements;
        }
    else
      let gi = ref None in
      let len = Array.length font_info.glyph_info_with_char in
      for glyph_info_index = 0 to len - 1 do
        let c', gi' = font_info.glyph_info_with_char.(glyph_info_index) in
        if c' = c then gi := Some gi'
      done;
      let gi = Option.get !gi in
      let x_advance = gi.x_advance in
      let wraps = acc.x + x_advance > bbox.x + bbox.width in
      let new_x, new_y =
        if wraps then
          (digits_widths_summed + x_advance, acc.y + font_info.font_height)
        else (acc.x + x_advance, acc.y)
      in
      let y_pos = acc.y + (scroll_y_offset * font_info.font_height) in
      (* descender is a negative value *)
      let descender = font_info.descender in
      if
        y_pos <= bbox.y + bbox.height && y_pos >= 0 && Bytes.length gi.bytes > 0
      then
        write_to_text_buffer ~render_buf_container:text_buffer
          ~x:(if wraps then digits_widths_summed else acc.x)
          ~y:(y_pos + descender + if wraps then font_info.font_height else 0)
          ~glyph_info:gi ~glyph:c ~font_info;
      Rope_Traversal_Info
        { acc with rope_pos = acc.rope_pos + 1; x = new_x; y = new_y }
  in
  traverse_rope rope fold_fn_for_drawing_text
    (Rope_Traversal_Info
       {
         line_number_placements = [ (1, font_info.font_height) ];
         rope_pos = 0;
         x = bbox.x + digits_widths_summed;
         y = bbox.y + font_info.font_height;
         line_num = 1;
       })

let ui_buffer : render_buffer_wrapper =
  {
    buffer =
      Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout
        (1000 * 1000 * _EACH_POINT_FLOAT_AMOUNT);
    length = 0;
  }

let cursor_buffer =
  {
    buffer =
      Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout
        (4 * _EACH_POINT_FLOAT_AMOUNT);
    length = 0;
  }

let _ = gl_enable_texture_2d ()
let _ = gl_enable_blending ()

let fragment =
  match gl_create_fragment_shader () with Ok f -> f | Error e -> failwith e

let vertex =
  match gl_create_vertex_shader () with Ok v -> v | Error e -> failwith e

let vertex_cursor =
  match gl_create_vertex_shader () with
  | Ok v -> v
  | Error e -> failwith (e ^ "_CURSOR")

let fragment_cursor =
  match gl_create_fragment_shader () with
  | Ok f -> f
  | Error e -> failwith (e ^ "_CURSOR")

let vertex_highlight =
  match gl_create_vertex_shader () with Ok v -> v | Error e -> failwith e

let fragment_highlight =
  match gl_create_fragment_shader () with Ok v -> v | Error e -> failwith e

let text_vertex_id =
  match gl_create_vertex_shader () with
  | Ok v -> v
  | Error e -> failwith (e ^ "; couldn't create vertex shader for text")

let text_fragment_id =
  match gl_create_fragment_shader () with
  | Ok v -> v
  | Error e -> failwith (e ^ "; couldn't create vertex shader for text")

let text_shader_program =
  compile_shaders_and_return_program ~vertex_id:text_vertex_id
    ~fragment_id:text_fragment_id ~vertex_src:text_vertex_shader
    ~fragment_src:text_fragment_shader

let program =
  compile_shaders_and_return_program ~vertex_id:vertex ~fragment_id:fragment
    ~vertex_src:generic_vertex_shader ~fragment_src:generic_fragment_shader

(* At first, it seems like there could be a write_rope_to_text_buffer function, BUT
  there are specific details like wrapping that I'd like to handle. Maybe there could be
  an abstraction for that specific wrapping behavior, but let's consider that later.
*)
let draw_text_editor ~(font_info : Ui.font_info) ~rope ~(scroll_y_offset : int)
    ~(bbox : Ui.bounding_box) ~highlight ~cursor_pos =
  let window_dims = Sdl.sdl_gl_getdrawablesize () in
  let window_width, window_height = window_dims in
  match rope with
  | Some r ->
      let lines = num_lines r in
      let digits_widths_summed =
        get_digits_widths_summed ~num_lines:lines ~font_info
      in
      let Editor.{ line_number_placements; _ } =
        draw_text ~font_info ~rope:r ~window_width ~window_height ~bbox
          ~digits_widths_summed ~scroll_y_offset ~text_buffer:ui_buffer
      in
      draw_line_numbers ~font_info ~scroll_y_offset ~window_width ~bbox
        ~window_height ~text_buffer:ui_buffer ~line_number_placements;
      draw_highlight ~r ~scroll_y_offset ~highlight ~bbox ~font_info
        ~window_width ~window_height ~highlight_buffer:ui_buffer
        ~digits_widths_summed;
      draw_cursor ~r ~cursor_buffer ~cursor_pos ~scroll_y_offset ~window_width
        ~window_height ~digits_widths_summed ~font_info ~bbox
  | None -> ()

let gl_buffer_obj = gl_gen_one_buffer ()
let gl_buffer_cursor = gl_gen_one_buffer ()
let gl_buffer_highlight = gl_gen_one_buffer ()
let gl_buffer_glyph_texture_atlas = gl_gen_texture ()

let location_point_vertex =
  match gl_getattriblocation program "point_vertex" with
  | Ok l -> l
  | Error e -> failwith e

let location_color =
  match gl_getattriblocation program "color_attrib" with
  | Ok l -> l
  | Error e -> failwith e

let vertex_text_location =
  match gl_getattriblocation text_shader_program "vertex" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

let color_text_location =
  match gl_getattriblocation text_shader_program "color" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

let tex_coord_text_location =
  match gl_getattriblocation text_shader_program "tex_coord" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

let sampler_text_location =
  match gl_getuniformlocation text_shader_program "sampler" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

let setup_glyph_texture ~(font_info : Ui.font_info) =
  gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
  set_gl_tex_parameters ();
  gl_teximage_2d ~bytes:font_info.font_texture_atlas.bytes
    ~width:font_info.font_texture_atlas.width
    ~height:font_info.font_texture_atlas.height

let () =
  gl_enable_vertex_attrib_array vertex_text_location;
  gl_enable_vertex_attrib_array color_text_location;
  gl_enable_vertex_attrib_array tex_coord_text_location;
  gl_enable_vertex_attrib_array location_point_vertex;
  gl_enable_vertex_attrib_array location_color;
  gl_bind_buffer gl_buffer_obj;
  gl_buffer_data_big_array ~render_buffer:ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim ui_buffer.buffer);
  gl_bind_buffer gl_buffer_cursor;
  gl_buffer_data_big_array ~render_buffer:cursor_buffer.buffer
    ~capacity:(Bigarray.Array1.dim cursor_buffer.buffer);
  gl_bind_buffer gl_buffer_highlight;
  gl_buffer_data_big_array ~render_buffer:ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim ui_buffer.buffer)

let draw ~(rope : Rope.rope option) ~cursor_pos ~highlight_pos ~scroll_y_offset
    ~scroll_x_offset ~font_info ~bbox =
  gl_clear_color 1. 1. 1. 1.;
  gl_clear ();

  draw_text_editor ~font_info ~rope ~cursor_pos ~highlight:highlight_pos ~bbox
    ~scroll_y_offset;

  gl_bind_buffer gl_buffer_cursor;

  gl_use_program program;

  gl_vertex_attrib_pointer_float_type ~location:location_point_vertex ~size:2
    ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

  gl_vertex_attrib_pointer_float_type ~location:location_color ~size:4
    ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;

  gl_buffer_subdata_big_array ~render_buffer:cursor_buffer.buffer
    ~length:cursor_buffer.length;

  gl_draw_arrays_with_quads (cursor_buffer.length / _EACH_POINT_FLOAT_AMOUNT);

  cursor_buffer.length <- 0;

  gl_bind_buffer gl_buffer_obj;

  (* 0 is default texture unit; reminder that sampler is not location but texture unit *)
  gl_uniform_1i ~location:sampler_text_location ~value:0;

  gl_use_program text_shader_program;

  gl_vertex_attrib_pointer_float_type ~location:vertex_text_location ~size:2
    ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:0;

  gl_vertex_attrib_pointer_float_type ~location:color_text_location ~size:3
    ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:2;

  gl_vertex_attrib_pointer_float_type ~location:tex_coord_text_location ~size:2
    ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:5;

  gl_buffer_subdata_big_array ~render_buffer:ui_buffer.buffer
    ~length:ui_buffer.length;

  gl_draw_arrays_with_quads (ui_buffer.length / _EACH_POINT_FLOAT_AMOUNT);

  Bigarray.Array1.fill ui_buffer.buffer 0.;

  ui_buffer.length <- 0;

  Sdl.sdl_gl_swapwindow Sdl.w
