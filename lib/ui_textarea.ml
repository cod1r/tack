open Sdl
open Ui_types

let _LINE_NUMBER_RIGHT_PADDING = 20

let create_textarea_box ?(text : Rope_types.rope option) () =
  { Ui.default_box with
    focusable = true
  ; clip_content = true
  ; content = Some (Textarea { Ui.default_text_area_information with text })
  ; allow_vertical_scroll = true
  ; allow_horizontal_scroll = true
  ; on_event = Some Ui.default_textarea_event_handler
  }
;;

let find_closest_vertical_range ~(box : box) ~(font_info : Freetype.font_info) ~rope ~y =
  let bbox =
    match box.bbox with
    | Some bbox -> bbox
    | None -> failwith ("should have bbox;" ^ __LOC__)
  in
  let lower_y = bbox.y in
  let upper_y = lower_y + font_info.font_height in
  let _, Rope_types.{ closest_vertical_range; _ } =
    Rope.traverse_rope
      ~box
      ~font_info
      ~rope
      ~handle_result:None
      ~result:
        (Rope_types.Finding_Cursor
           ( { x = bbox.x; y = lower_y; rope_pos = 0 }
           , { upper_y
             ; closest_col = None
             ; closest_rope = None
             ; closest_vertical_range = None
             ; original_pos = Y y
             } ))
  in
  closest_vertical_range
;;

let find_closest_horizontal_pos
      ~(box : box)
      ~(font_info : Freetype.font_info)
      ~rope
      ~x
      ~closest_vertical_range
  =
  let bbox =
    match box.bbox with
    | Some bbox -> bbox
    | None -> failwith ("should have bbox;" ^ __LOC__)
  in
  let start_x = bbox.x in
  let lower_y = bbox.y in
  let upper_y = lower_y + font_info.font_height in
  let rope_traversal_info, closest_info =
    Rope.traverse_rope
      ~box
      ~font_info
      ~rope
      ~handle_result:None
      ~result:
        (Rope_types.Finding_Cursor
           ( { x = start_x; y = lower_y; rope_pos = 0 }
           , { closest_rope = None
             ; closest_col = None
             ; upper_y
             ; closest_vertical_range
             ; original_pos = X x
             } ))
  in
  let _, closest_rope =
    Rope.get_pair_col_and_rope_pos ~rope_traversal_info ~closest_info ~x
  in
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
      ~(box : box)
      ~(font_info : Freetype.font_info)
      ~x
      ~y
      ~rope
  =
  let width_ratio, height_ratio = Sdl.get_logical_to_opengl_window_dims_ratio () in
  (* ratio is needed because the x,y coords given from MouseEvent is based on window without high dpi so scaling needs to happen
  and offsetting the scroll offsets *)
  let x = (x * width_ratio) - box.scroll_x_offset
  and y = (y * height_ratio) - box.scroll_y_offset in
  let closest_vertical_range = find_closest_vertical_range ~box ~font_info ~rope ~y in
  let closest_rope =
    find_closest_horizontal_pos ~box ~font_info ~rope ~closest_vertical_range ~x
  in
  if closest_rope = None then Rope.length rope else closest_rope |> Option.get
;;

let find_coords_for_cursor_pos ~(box : box) ~font_info ~rope ~cursor_pos =
  assert (box.bbox <> None);
  let bbox = Option.get box.bbox in
  let found = ref None in
  let fn_for_finding_coords (Rope_types.Rope_Traversal_Info acc) _c =
    if acc.rope_pos = cursor_pos then found := Some acc
  in
  ignore
    (Rope.traverse_rope
       ~box
       ~font_info
       ~rope
       ~handle_result:(Some fn_for_finding_coords)
       ~result:(Rope_types.Rope_Traversal_Info { x = bbox.x; y = bbox.y; rope_pos = 0 }));
  match !found with
  | Some acc -> acc
  | None -> "EXPECTED TO HAVE SOME COORDS " ^ __LOC__ |> failwith
;;

let find_closest_rope_pos_for_moving_cursor_in_vertical_range
      ~(font_info : Freetype.font_info)
      ~cursor_x
      ~lower_y
      ~rope
      ~box
  =
  let hor_pos =
    find_closest_horizontal_pos
      ~font_info
      ~rope
      ~x:cursor_x
      ~box
      ~closest_vertical_range:(Some (lower_y, lower_y + font_info.font_height))
  in
  hor_pos
;;

let handle_kbd_evt
      ~(font_info : Freetype.font_info)
      ~char_code
      ~box
      ~kbd_evt_type
      ~keysym
      ~(text_area_information : text_area_information)
  : text_area_information
  =
  let r = Option.value text_area_information.text ~default:(Rope.of_string "") in
  match char_code with
  | 1073742048 ->
    (* this is the integer encoding for ctrl in SDL *)
    text_area_information
  | 1073741904 (* left arrow key *) when kbd_evt_type = Keydown ->
    let minus_1 =
      max 0 (pred (Option.value text_area_information.cursor_pos ~default:0))
    in
    { text_area_information with cursor_pos = Some minus_1; highlight_pos = None, None }
  | (1073741906 (* up arrow key *) | 1073741905 (* down arrow key *))
    when kbd_evt_type = Keydown ->
    let Rope_types.{ x; y; _ } =
      find_coords_for_cursor_pos
        ~box
        ~font_info
        ~rope:r
        ~cursor_pos:(Option.value text_area_information.cursor_pos ~default:0)
    in
    let cursor_pos' =
      find_closest_rope_pos_for_moving_cursor_in_vertical_range
        ~rope:r
        ~box
        ~font_info
        ~cursor_x:x
        ~lower_y:
          ((if char_code = 1073741906 then ( - ) else ( + )) y font_info.font_height)
    in
    if cursor_pos' = None
    then text_area_information
    else
      { text_area_information with cursor_pos = cursor_pos'; highlight_pos = None, None }
  | 1073741903 (* right arrow key *) when kbd_evt_type = Keydown ->
    let plus_1 =
      min
        (Rope.length r)
        (succ (Option.value text_area_information.cursor_pos ~default:0))
    in
    { text_area_information with cursor_pos = Some plus_1; highlight_pos = None, None }
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
           let new_rope = Some (Rope.delete r ~start:(max 0 (cursor_pos - 1)) ~len:1) in
           { text_area_information with
             text = new_rope
           ; cursor_pos = Some (cursor_pos - 1)
           ; highlight_pos = None, None
           })
       else text_area_information
     | 'c' when kbd_evt_type = Keydown && !Ui_globals.holding_ctrl ->
       (match text_area_information.highlight_pos with
        | Some start, Some end' ->
          Rope.substring r ~start ~len:(end' - start)
          |> Rope.to_string
          |> Sdl.set_clipboard_text
        | _ -> ());
       text_area_information
     | 'v' when kbd_evt_type = Keydown && !Ui_globals.holding_ctrl ->
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
       { text_area_information with text = new_rope; cursor_pos = Some (cursor_pos' + 1) }
     | '\t' when kbd_evt_type = Keydown ->
       (* horizontal tab will be two spaces for now *)
       let cursor_pos' =
         Option.value text_area_information.cursor_pos ~default:(Rope.length r)
       in
       let new_rope = Some (Rope.insert r cursor_pos' "  ") in
       { text_area_information with text = new_rope; cursor_pos = Some (cursor_pos' + 2) }
     | _ -> text_area_information)
;;

let handle_txt_evt ~(text_area_information : text_area_information) ~text =
  let r = Option.value text_area_information.text ~default:(Rope.of_string "") in
  let cursor_pos' =
    Option.value text_area_information.cursor_pos ~default:(Rope.length r)
  in
  let cursor_pos', new_rope =
    match text_area_information.highlight_pos with
    | Some start, Some end' -> start, Rope.delete r ~start ~len:(end' - start)
    | _ -> cursor_pos', r
  in
  let new_rope = Rope.insert new_rope cursor_pos' text in
  { text = Some new_rope
  ; highlight_pos = None, None
  ; holding_mousedown_rope_pos = None
  ; cursor_pos = Some (cursor_pos' + String.length text)
  }
;;

let handle_mouse_motion_evt
      ~(text_area_information : text_area_information)
      ~x
      ~y
      ~box
      ~font_info
      ~rope
  =
  let rope = Option.value rope ~default:(Rope.of_string "") in
  match text_area_information.holding_mousedown_rope_pos with
  | Some mousedown_rope_pos ->
    let cursor_pos' =
      find_closest_rope_pos_for_cursor_on_coords ~box ~font_info ~x ~y ~rope
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
