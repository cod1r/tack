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

type _ traverse_info =
  | Rope_Traversal_Info :
      rope_traversal_info_
      -> rope_traversal_info_ traverse_info
  | Num_Lines : int -> int traverse_info
  | Finding_Cursor : closest_information -> closest_information traverse_info

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
  let fold_fn_for_finding_coords (acc : rope_traversal_info_ traverse_info) c =
    let (Rope_Traversal_Info acc) = acc in
    if acc.rope_pos != cursor_pos then
      let new_x, new_y =
        calc_new_xy ~bbox ~font_info ~x:acc.x ~y:acc.y ~digits_widths_summed
          ~char:c
      in
      Rope_Traversal_Info
        { acc with x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
    else Rope_Traversal_Info acc
  in
  traverse_rope rope fold_fn_for_finding_coords
    (Rope_Traversal_Info
       {
         x = bbox.x + digits_widths_summed;
         y = bbox.y + (scroll_y_offset * font_info.font_height);
         (* these three aren't used here but are needed for the type *)
         rope_pos = 0;
         line_num = 0;
         line_number_placements = [];
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

let write_to_cursor_buffer ~(cursor_buffer : Ui_rendering.render_buffer_wrapper) ~x ~y
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
            Ui_rendering.write_to_text_buffer ~font_info
            ~render_buf_container:text_buffer
              ~glyph_info:gi ~x:!curr_x_offset
              ~y:(y_pos + font_info.descender)
              ~glyph:c';
            curr_x_offset := !curr_x_offset + gi.FreeType.x_advance)
          glyph_infos)
    line_number_placements

let draw_cursor ~(font_info : Ui.font_info) ~(bbox : Ui.bounding_box)
    ~(r : Rope.rope) ~cursor_pos ~(cursor_buffer : Ui_rendering.render_buffer_wrapper)
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
        Ui_rendering.write_to_text_buffer ~render_buf_container:text_buffer
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

let cursor_buffer: Ui_rendering.render_buffer_wrapper =
  {
    buffer =
      Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout
        (4 * Ui_rendering._EACH_POINT_FLOAT_AMOUNT);
    length = 0;
  }

let _ = gl_enable_texture_2d ()
let _ = gl_enable_blending ()

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
          ~digits_widths_summed ~scroll_y_offset ~text_buffer:Ui_rendering.ui_buffer
      in
      draw_line_numbers ~font_info ~scroll_y_offset ~window_width ~bbox
        ~window_height ~text_buffer:Ui_rendering.ui_buffer ~line_number_placements;
      draw_highlight ~r ~scroll_y_offset ~highlight ~bbox ~font_info
        ~window_width ~window_height ~highlight_buffer:Ui_rendering.ui_buffer
        ~digits_widths_summed;
      draw_cursor ~r ~cursor_buffer ~cursor_pos ~scroll_y_offset ~window_width
        ~window_height ~digits_widths_summed ~font_info ~bbox
  | None -> ()

let gl_buffer_obj = gl_gen_one_buffer ()
let gl_buffer_cursor = gl_gen_one_buffer ()
let gl_buffer_highlight = gl_gen_one_buffer ()
let gl_buffer_glyph_texture_atlas = gl_gen_texture ()

let setup_glyph_texture ~(font_info : Ui.font_info) =
  gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
  set_gl_tex_parameters ();
  gl_teximage_2d ~bytes:font_info.font_texture_atlas.bytes
    ~width:font_info.font_texture_atlas.width
    ~height:font_info.font_texture_atlas.height

let () =
  gl_enable_vertex_attrib_array Ui_rendering.vertex_text_location;
  gl_enable_vertex_attrib_array Ui_rendering.color_text_location;
  gl_enable_vertex_attrib_array Ui_rendering.tex_coord_text_location;
  gl_enable_vertex_attrib_array Ui_rendering.location_point_vertex;
  gl_enable_vertex_attrib_array Ui_rendering.location_color;
  gl_bind_buffer gl_buffer_obj;
  gl_buffer_data_big_array ~render_buffer:Ui_rendering.ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim Ui_rendering.ui_buffer.buffer);
  gl_bind_buffer gl_buffer_cursor;
  gl_buffer_data_big_array ~render_buffer:cursor_buffer.buffer
    ~capacity:(Bigarray.Array1.dim cursor_buffer.buffer);
  gl_bind_buffer gl_buffer_highlight;
  gl_buffer_data_big_array ~render_buffer:Ui_rendering.ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim Ui_rendering.ui_buffer.buffer)

let draw ~(rope : Rope.rope option) ~cursor_pos ~highlight_pos ~scroll_y_offset
    ~scroll_x_offset ~font_info ~bbox =
  gl_clear_color 1. 1. 1. 1.;
  gl_clear ();

  draw_text_editor ~font_info ~rope ~cursor_pos ~highlight:highlight_pos ~bbox
    ~scroll_y_offset;

  gl_bind_buffer gl_buffer_cursor;

  gl_use_program Ui_rendering.program;

  gl_vertex_attrib_pointer_float_type ~location:Ui_rendering.location_point_vertex ~size:2
    ~stride:Ui_rendering._EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

  gl_vertex_attrib_pointer_float_type ~location:Ui_rendering.location_color ~size:4
    ~stride:Ui_rendering._EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;

  gl_buffer_subdata_big_array ~render_buffer:cursor_buffer.buffer
    ~length:cursor_buffer.length;

  gl_draw_arrays_with_quads (cursor_buffer.length / Ui_rendering._EACH_POINT_FLOAT_AMOUNT);

  cursor_buffer.length <- 0;

  gl_bind_buffer gl_buffer_obj;

  (* 0 is default texture unit; reminder that sampler is not location but texture unit *)
  gl_uniform_1i ~location:Ui_rendering.sampler_text_location ~value:0;

  gl_use_program Ui_rendering.text_shader_program;

  gl_vertex_attrib_pointer_float_type ~location:Ui_rendering.vertex_text_location ~size:2
    ~stride:Ui_rendering._EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:0;

  gl_vertex_attrib_pointer_float_type ~location:Ui_rendering.color_text_location ~size:3
    ~stride:Ui_rendering._EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:2;

  gl_vertex_attrib_pointer_float_type ~location:Ui_rendering.tex_coord_text_location ~size:2
    ~stride:Ui_rendering._EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:5;

  gl_buffer_subdata_big_array ~render_buffer:Ui_rendering.ui_buffer.buffer
    ~length:Ui_rendering.ui_buffer.length;

  gl_draw_arrays_with_quads (Ui_rendering.ui_buffer.length / Ui_rendering._EACH_POINT_FLOAT_AMOUNT);

  Bigarray.Array1.fill Ui_rendering.ui_buffer.buffer 0.;

  Ui_rendering.ui_buffer.length <- 0;

  Sdl.sdl_gl_swapwindow Sdl.w
