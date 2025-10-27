open Freetype
open Sdl
open Opengl

let _LINE_NUMBER_RIGHT_PADDING = 20

type rope_traversal_info =
  { x : int
  ; y : int
  ; rope_pos : int
  }

type closest_information =
  { closest_col : int option
  ; upper_y : int
  ; closest_vertical_range : (int * int) option
  ; closest_rope : int option
  }

type _ traverse_info =
  | Rope_Traversal_Info : rope_traversal_info -> rope_traversal_info traverse_info
  | Num_Lines : int -> int traverse_info
  | Finding_Cursor :
      (rope_traversal_info * closest_information)
      -> (rope_traversal_info * closest_information) traverse_info

let rec traverse_rope
  : type a.
    rope:Rope.rope
    -> handle_result:(a traverse_info -> char -> a traverse_info)
    -> result:a traverse_info
    -> a
  =
  fun ~(rope : Rope.rope)
    ~(handle_result : a traverse_info -> char -> a traverse_info)
    ~(result : a traverse_info) ->
  match rope with
  | Leaf l ->
    (match result with
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
         let (Finding_Cursor temp) = handle_result (Finding_Cursor !acc) l.[i] in
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
    let left_result = traverse_rope ~rope:left ~handle_result ~result in
    let right_result =
      traverse_rope
        ~rope:right
        ~handle_result
        ~result:
          (match result with
           | Rope_Traversal_Info _ -> Rope_Traversal_Info left_result
           | Finding_Cursor _ -> Finding_Cursor left_result
           | Num_Lines _ -> Num_Lines left_result)
    in
    right_result
;;

let num_lines (rope : Rope.rope) =
  let fold_fn (acc : int traverse_info) ch =
    if ch = '\n'
    then (
      let (Num_Lines l) = acc in
      Num_Lines (l + 1))
    else acc
  in
  let accumulation = traverse_rope ~rope ~handle_result:fold_fn ~result:(Num_Lines 0) in
  accumulation
;;

let get_pair_col_and_rope_pos ~rope_traversal_info ~closest_info ~x =
  match closest_info.closest_vertical_range with
  | Some (s, e) ->
    if rope_traversal_info.y = s && closest_info.upper_y = e
    then (
      match closest_info.closest_col with
      | Some closest_col ->
        if abs (closest_col - x) < abs (rope_traversal_info.x - x)
        then closest_info.closest_col, closest_info.closest_rope
        else Some rope_traversal_info.x, Some rope_traversal_info.rope_pos
      | None -> Some rope_traversal_info.x, Some rope_traversal_info.rope_pos)
    else closest_info.closest_col, closest_info.closest_rope
  | None -> None, None
;;

let find_closest_vertical_range
      ~(bbox : Ui.bounding_box)
      ~(font_info : Ui.font_info)
      ~rope
      ~y
      ~scroll_y_offset
  =
  let fold_fn_for_vertical_range closest_info c =
    let (Finding_Cursor (rope_traversal_info, closest_info)) = closest_info in
    match c with
    | '\n' ->
      Finding_Cursor
        ( { rope_traversal_info with
            x = bbox.x
          ; y = rope_traversal_info.y + font_info.font_height
          }
        , { closest_info with
            upper_y = closest_info.upper_y + font_info.font_height
          ; closest_vertical_range =
              (if y >= rope_traversal_info.y && y <= closest_info.upper_y
               then Some (rope_traversal_info.y, closest_info.upper_y)
               else closest_info.closest_vertical_range)
          } )
    | _ ->
      let gi = Ui.get_glyph_info_from_glyph ~glyph:c ~font_info in
      let x_advance = gi.x_advance in
      let new_x, new_y =
        if rope_traversal_info.x + x_advance > bbox.x + bbox.width
        then bbox.x, rope_traversal_info.y + font_info.font_height
        else rope_traversal_info.x + x_advance, rope_traversal_info.y
      in
      Finding_Cursor
        ( { rope_traversal_info with x = new_x; y = new_y }
        , { closest_info with
            upper_y = new_y + font_info.font_height
          ; closest_vertical_range =
              (if y >= rope_traversal_info.y && y <= closest_info.upper_y
               then Some (rope_traversal_info.y, closest_info.upper_y)
               else closest_info.closest_vertical_range)
          } )
  in
  let lower_y = bbox.y + (scroll_y_offset * font_info.font_height) in
  let upper_y = lower_y + font_info.font_height in
  let (_, { closest_vertical_range; _ }) : rope_traversal_info * closest_information =
    traverse_rope
      ~rope
      ~handle_result:fold_fn_for_vertical_range
      ~result:
        (Finding_Cursor
           ( { x = bbox.x; y = lower_y; rope_pos = 0 }
           , { upper_y
             ; closest_col = None
             ; closest_rope = None
             ; closest_vertical_range = None
             } ))
  in
  closest_vertical_range
;;

let find_closest_horizontal_pos
      ~(bbox : Ui.bounding_box)
      ~(font_info : Ui.font_info)
      ~rope
      ~x
      ~scroll_y_offset
      ~closest_vertical_range
  =
  let fold_fn_for_close_x closest_info c =
    let (Finding_Cursor (rope_traversal_info, closest_info)) = closest_info in
    match c with
    | '\n' ->
      let closest_col, closest_rope =
        get_pair_col_and_rope_pos ~rope_traversal_info ~closest_info ~x
      in
      Finding_Cursor
        ( { x = bbox.x
          ; y = rope_traversal_info.y + font_info.font_height
          ; rope_pos = rope_traversal_info.rope_pos + 1
          }
        , { closest_info with
            upper_y = closest_info.upper_y + font_info.font_height
          ; closest_col
          ; closest_rope
          } )
    | _ ->
      let ~new_x, ~new_y, .. =
        Ui.get_text_wrap_info
          ~bbox
          ~glyph:c
          ~font_info
          ~x:rope_traversal_info.x
          ~y:rope_traversal_info.y
      in
      let closest_col, closest_rope =
        get_pair_col_and_rope_pos ~rope_traversal_info ~closest_info ~x
      in
      Finding_Cursor
        ( { x = new_x; y = new_y; rope_pos = rope_traversal_info.rope_pos + 1 }
        , { closest_info with
            upper_y = new_y + font_info.font_height
          ; closest_col
          ; closest_rope
          } )
  in
  let lower_y = bbox.y + (scroll_y_offset * font_info.font_height) in
  let upper_y = lower_y + font_info.font_height in
  let rope_traversal_info, closest_info =
    traverse_rope
      ~rope
      ~handle_result:fold_fn_for_close_x
      ~result:
        (Finding_Cursor
           ( { x = bbox.x; y = lower_y; rope_pos = 0 }
           , { closest_rope = None; closest_col = None; upper_y; closest_vertical_range }
           ))
  in
  let _, closest_rope = get_pair_col_and_rope_pos ~rope_traversal_info ~closest_info ~x in
  closest_rope
;;

(*
   The algorithm for finding the closest rope position in the rope data structure
     given an x and a y from mousedown event:

       1. Find the vertical range that the y value resides in by traversing the rope and
          building up x and y values according to the screen coordinates. Top left is the origin
          and far right is window_width (drawable size) and bottom most is window_height (drawable size).
       2. Find the horizontal x position, that is built from traversing the rope again given the vertical range,
          that is within the vertical range and is closest to the x value for the mousedown event.
*)
let find_closest_rope_pos_for_cursor_on_coords
      ~(bbox : Ui.bounding_box)
      ~(font_info : Ui.font_info)
      ~x
      ~y
      ~rope
      ~scroll_y_offset
  =
  let width_ratio, height_ratio = Ui.get_logical_to_opengl_window_dims_ratio () in
  (* ratio is needed because the x,y coords given from MouseEvent is based on window without high dpi so scaling needs to happen *)
  let x = x * width_ratio
  and y = y * height_ratio in
  let closest_vertical_range =
    find_closest_vertical_range ~bbox ~font_info ~rope ~scroll_y_offset ~y
  in
  let closest_rope =
    find_closest_horizontal_pos
      ~bbox
      ~font_info
      ~rope
      ~scroll_y_offset
      ~closest_vertical_range
      ~x
  in
  if closest_rope = None then Rope.length rope else closest_rope |> Option.get
;;

let find_coords_for_cursor_pos
      ~(font_info : Ui.font_info)
      ~(bbox : Ui.bounding_box)
      ~rope
      ~cursor_pos
      ~scroll_y_offset
  =
  let fold_fn_for_finding_coords acc c =
    let (Rope_Traversal_Info acc) = acc in
    if acc.rope_pos != cursor_pos
    then (
      let ~new_x, ~new_y, .. =
        Ui.get_text_wrap_info ~bbox ~font_info ~x:acc.x ~y:acc.y ~glyph:c
      in
      Rope_Traversal_Info { x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 })
    else Rope_Traversal_Info acc
  in
  traverse_rope
    ~rope
    ~handle_result:fold_fn_for_finding_coords
    ~result:
      (Rope_Traversal_Info
         { x = bbox.x
         ; y = bbox.y + (scroll_y_offset * font_info.font_height)
         ; rope_pos = 0
         })
;;

let find_closest_rope_pos_for_moving_cursor_in_vertical_range
      ~(font_info : Ui.font_info)
      ~cursor_x
      ~lower_y
      ~rope
      ~scroll_y_offset
  =
  let hor_pos =
    find_closest_horizontal_pos
      ~font_info
      ~rope
      ~x:cursor_x
      ~scroll_y_offset
      ~closest_vertical_range:(Some (lower_y, lower_y + font_info.font_height))
  in
  hor_pos
;;

let handle_kbd_evt
      ~(font_info : Ui.font_info)
      ~char_code
      ~bbox
      ~kbd_evt_type
      ~keysym
      ~(text_area_information : Ui.text_area_information)
  : Ui.text_area_information
  =
  match text_area_information.text with
  | Some r ->
    (match char_code with
     | 1073742048 ->
       (* this is the integer encoding for ctrl in SDL *)
       (match kbd_evt_type with
        | Sdl.Keydown -> { text_area_information with holding_ctrl = true }
        | Keyup -> { text_area_information with holding_ctrl = false })
     | 1073741904 (* left arrow key *) when kbd_evt_type = Keydown ->
       let minus_1 =
         max 0 (pred (Option.value text_area_information.cursor_pos ~default:0))
       in
       { text_area_information with cursor_pos = Some minus_1 }
     | (1073741906 (* up arrow key *) | 1073741905 (* down arrow key *))
       when kbd_evt_type = Keydown ->
       let { x; y; _ } =
         find_coords_for_cursor_pos
           ~bbox
           ~font_info
           ~rope:r
           ~cursor_pos:(Option.value text_area_information.cursor_pos ~default:0)
           ~scroll_y_offset:text_area_information.scroll_y_offset
       in
       let cursor_pos' =
         find_closest_rope_pos_for_moving_cursor_in_vertical_range
           ~rope:r
           ~bbox
           ~font_info
           ~scroll_y_offset:text_area_information.scroll_y_offset
           ~cursor_x:x
           ~lower_y:
             ((if char_code = 1073741906 then ( - ) else ( + )) y font_info.font_height)
       in
       { text_area_information with cursor_pos = cursor_pos' }
     | 1073741903 (* right arrow key *) when kbd_evt_type = Keydown ->
       let plus_1 =
         min
           (Rope.length r)
           (succ (Option.value text_area_information.cursor_pos ~default:0))
       in
       { text_area_information with cursor_pos = Some plus_1 }
     | _ ->
       (match keysym with
        | '\b' when kbd_evt_type = Keydown ->
          (* backspace *)
          let rope_len = Rope.length r in
          if rope_len > 0
          then (
            let cursor_pos =
              Option.value text_area_information.cursor_pos ~default:(Rope.length r)
            in
            match text_area_information.highlight_pos with
            | Some start, Some end' when end' - start > 0 ->
              let new_rope = Some (Rope.delete r ~start ~len:(end' - start)) in
              { text_area_information with
                text = new_rope
              ; cursor_pos = Some start
              ; highlight_pos = None, None
              }
            | _ ->
              let new_rope =
                Some (Rope.delete r ~start:(max 0 (cursor_pos - 1)) ~len:1)
              in
              { text_area_information with
                text = new_rope
              ; cursor_pos = Some (cursor_pos - 1)
              ; highlight_pos = None, None
              })
          else text_area_information
        | 'c' when kbd_evt_type = Keydown && text_area_information.holding_ctrl ->
          (match text_area_information.highlight_pos with
           | Some start, Some end' ->
             Rope.substring r ~start ~len:(end' - start)
             |> Rope.to_string
             |> Sdl.set_clipboard_text
           | _ -> ());
          text_area_information
        | 'v' when kbd_evt_type = Keydown && text_area_information.holding_ctrl ->
          let clipboard_contents = Sdl.get_clipboard_text () in
          let new_rope =
            Rope.insert
              r
              (Option.value text_area_information.cursor_pos ~default:(Rope.length r))
              clipboard_contents
          in
          { text_area_information with text = Some new_rope }
        | ('\r' | '\n') when kbd_evt_type = Keydown ->
          (* on macos, the return key gives \r instead of \n *)
          let cursor_pos' =
            Option.value text_area_information.cursor_pos ~default:(Rope.length r)
          in
          let new_rope = Some (Rope.insert r cursor_pos' "\n") in
          { text_area_information with
            text = new_rope
          ; cursor_pos = Some (cursor_pos' + 1)
          }
        | '\t' when kbd_evt_type = Keydown ->
          (* horizontal tab will be two spaces for now *)
          let cursor_pos' =
            Option.value text_area_information.cursor_pos ~default:(Rope.length r)
          in
          let new_rope = Some (Rope.insert r cursor_pos' "  ") in
          { text_area_information with
            text = new_rope
          ; cursor_pos = Some (cursor_pos' + 2)
          }
        | _ -> text_area_information))
  | None -> text_area_information
;;

let handle_txt_evt ~(text_area_information : Ui.text_area_information) ~text =
  match text_area_information.text with
  | Some r ->
    let cursor_pos' =
      Option.value text_area_information.cursor_pos ~default:(Rope.length r)
    in
    let cursor_pos', new_rope =
      match text_area_information.highlight_pos with
      | Some start, Some end' -> start, Rope.delete r ~start ~len:(end' - start)
      | _ -> cursor_pos', r
    in
    let new_rope = Rope.insert new_rope cursor_pos' text in
    { text_area_information with
      text = Some new_rope
    ; highlight_pos = None, None
    ; holding_mousedown_rope_pos = None
    ; cursor_pos = Some (cursor_pos' + String.length text)
    }
  | None -> { text_area_information with text = Some (text |> Rope.of_string) }
;;

let handle_mouse_motion_evt
      ~(text_area_information : Ui.text_area_information)
      ~x
      ~y
      ~bbox
      ~font_info
      ~rope
  =
  let rope = Option.value rope ~default:(Rope.of_string "") in
  match text_area_information.holding_mousedown_rope_pos with
  | Some mousedown_rope_pos ->
    let cursor_pos' =
      find_closest_rope_pos_for_cursor_on_coords
        ~bbox
        ~font_info
        ~x
        ~y
        ~rope
        ~scroll_y_offset:text_area_information.scroll_y_offset
    in
    { text_area_information with
      highlight_pos =
        (match text_area_information.highlight_pos with
         | Some _, Some _ ->
           if cursor_pos' <= mousedown_rope_pos
           then Some cursor_pos', Some mousedown_rope_pos
           else Some mousedown_rope_pos, Some cursor_pos'
         | Some start, None -> Some start, Some cursor_pos'
         | _ -> Some cursor_pos', None)
    ; cursor_pos = Some cursor_pos'
    }
  | None -> text_area_information
;;
