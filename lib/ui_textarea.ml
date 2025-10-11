open Freetype
open Sdl
open Opengl

let _LINE_NUMBER_RIGHT_PADDING = 20

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
    ~(font_info : Ui.font_info) ~rope ~y ~scroll_y_offset =
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
            x = bbox.x;
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
            (bbox.x, closest_info.lower_y + font_info.font_height)
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
           x = bbox.x;
           closest_rope = 0;
           rope_pos = 0;
           closest_vertical_range = None;
         })
  in
  closest_vertical_range

let find_closest_horizontal_pos ~(bbox : Ui.bounding_box)
    ~(font_info : Ui.font_info) ~rope ~x ~scroll_y_offset
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
            x = bbox.x;
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
            (bbox.x, closest_info.lower_y + font_info.font_height)
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
           x = bbox.x;
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
    ~(font_info : Ui.font_info) ~x ~y ~rope ~scroll_y_offset =
  let window_width, _ = Sdl.sdl_gl_getdrawablesize () in
  let window_width_without_high_dpi, _ = Sdl.sdl_get_window_size Sdl.w in
  (* ratio is needed because the x,y coords given from MouseEvent is based on window without high dpi so scaling needs to happen *)
  let ratio = window_width / window_width_without_high_dpi in
  let x = x * ratio and y = y * ratio in
  let closest_vertical_range =
    find_closest_vertical_range ~bbox ~font_info ~rope ~scroll_y_offset ~y
  in
  let closest_rope =
    find_closest_horizontal_pos ~bbox ~font_info ~rope ~scroll_y_offset
      ~closest_vertical_range ~x
  in
  if closest_rope = -1 then Rope.length rope else closest_rope

let calc_new_xy ~(bbox : Ui.bounding_box) ~(char : char)
    ~(font_info : Ui.font_info) ~x ~y =
  match char with
  | '\n' -> (bbox.x, y + font_info.font_height)
  | _ ->
      let _, gi =
        Array.find_opt (fun (c', _) -> c' = char) font_info.glyph_info_with_char
        |> Option.get
      in
      let x_advance = gi.x_advance in
      if x + x_advance > bbox.x + bbox.width then
        (bbox.x, y + font_info.font_height)
      else (x + x_advance, y)

let find_coords_for_cursor_pos ~(font_info : Ui.font_info)
    ~(bbox : Ui.bounding_box) ~rope ~cursor_pos ~scroll_y_offset =
  let num_lines = num_lines rope in
  let fold_fn_for_finding_coords (acc : rope_traversal_info_ traverse_info) c =
    let (Rope_Traversal_Info acc) = acc in
    if acc.rope_pos != cursor_pos then
      let new_x, new_y =
        calc_new_xy ~bbox ~font_info ~x:acc.x ~y:acc.y ~char:c
      in
      Rope_Traversal_Info
        { acc with x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
    else Rope_Traversal_Info acc
  in
  traverse_rope rope fold_fn_for_finding_coords
    (Rope_Traversal_Info
       {
         x = bbox.x;
         y = bbox.y + (scroll_y_offset * font_info.font_height);
         (* these three aren't used here but are needed for the type *)
         rope_pos = 0;
         line_num = 0;
         line_number_placements = [];
       })

let find_closest_rope_pos_for_moving_cursor_in_vertical_range
    ~(font_info : Ui.font_info) ~cursor_x ~lower_y ~rope ~scroll_y_offset =
  let num_lines = num_lines rope in
  let hor_pos =
    find_closest_horizontal_pos ~font_info ~rope ~x:cursor_x ~scroll_y_offset
      ~closest_vertical_range:(Some (lower_y, lower_y + font_info.font_height))
  in
  hor_pos
