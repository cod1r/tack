type bounding_box =
  { mutable width : int
  ; mutable height : int
  ; mutable x : int
  ; mutable y : int
  }

type direction =
  | Horizontal
  | Vertical

type box_sides =
  { left : int
  ; right : int
  ; top : int
  ; bottom : int
  }

type positioning =
  | Relative of
      { x : int
      ; y : int
      }
  | Absolute

type horizontal_alignment =
  | Left
  | Center
  | Right

type vertical_alignment =
  | Top
  | Center
  | Bottom

type size_constraint =
  | Min
  | Max

type box =
  { mutable name : string option
  ; mutable content : box_content option
  ; mutable bbox : bounding_box option
  ; mutable text_wrap : bool
  ; mutable background_color : float * float * float * float
  ; mutable border : bool
  ; mutable flow : direction option
  ; mutable font_size : int option
  ; mutable width_constraint : size_constraint option
  ; mutable height_constraint : size_constraint option
  ; mutable clip_content : bool
  ; mutable position_type : positioning
  ; mutable allow_horizontal_scroll : bool
  ; mutable allow_vertical_scroll : bool
  ; mutable horizontal_align : horizontal_alignment option
  ; mutable vertical_align : vertical_alignment option
  ; on_event : event_handler_t option
  ; scroll_x_offset : int
  ; scroll_y_offset : int
  }

and event_handler_t = b:box option -> e:Sdl.event -> unit

and text_area_information =
  { text : Rope.rope option
  ; cursor_pos : int option
  ; highlight_pos : int option * int option
  ; holding_mousedown_rope_pos : int option
  }

and box_content =
  | Box of box
  | Boxes of box list
  | Text of string
  | Textarea of text_area_information
  | ScrollContainer of
      { content : box
      ; scroll : box
      ; container : box
      }

let focused_element : box option ref = ref None
let set_focused_element ~(box : box) = focused_element := Some box
let unfocus_element () = focused_element := None
let default_bbox : bounding_box = { width = 0; height = 0; x = 0; y = 0 }

let holding_mousedown : [ `True of original_x:int * original_y:int | `False ] ref =
  ref `False
;;

let holding_ctrl = ref false

let get_box_sides ~(box : box) : box_sides =
  match box.bbox with
  | Some bbox ->
    let right = bbox.x + bbox.width
    and bottom = bbox.y + bbox.height in
    { left = bbox.x; top = bbox.y; right; bottom }
  | None -> failwith "calling get_box_sides requires a bbox property of Some"
;;

let get_logical_to_opengl_window_dims_ratio () =
  let window_width, window_height = Sdl.sdl_get_window_size Sdl.w
  and window_width_gl, window_height_gl = Sdl.sdl_gl_getdrawablesize () in
  window_width_gl / window_width, window_height_gl / window_height
;;

let default_text_area_information =
  { text = None
  ; cursor_pos = None
  ; highlight_pos = None, None
  ; holding_mousedown_rope_pos = None
  }
;;

let is_within_box ~x ~y ~box ~from_sdl_evt =
  let x, y =
    if from_sdl_evt
    then (
      let width_ratio, height_ratio = get_logical_to_opengl_window_dims_ratio () in
      x * width_ratio, y * height_ratio)
    else x, y
  in
  let { left; right; top; bottom } = get_box_sides ~box in
  x >= left && x <= right && y <= bottom && y >= top
;;

let default_textarea_event_handler =
  fun ~b ~e ->
  match b with
  | Some b ->
    (match b.content with
     | Some (Textarea info) ->
       (match e with
        | Sdl.MouseButtonEvt { x; y; _ } ->
          (match b.bbox with
           | Some _ ->
             if is_within_box ~x ~y ~from_sdl_evt:true ~box:b
             then set_focused_element ~box:b
           | None -> ())
        | _ -> ())
     | _ -> ())
  | _ -> ()
;;

let default_box =
  { name = None
  ; content = None
  ; bbox = None
  ; text_wrap = false
  ; background_color = 1., 1., 1., 1.
  ; border = false
  ; font_size = None
  ; width_constraint = None
  ; height_constraint = None
  ; clip_content = false
  ; position_type = Relative { x = 0; y = 0 }
  ; allow_horizontal_scroll = false
  ; allow_vertical_scroll = false
  ; horizontal_align = None
  ; vertical_align = None
  ; flow = None
  ; on_event = None
  ; scroll_x_offset = 0
  ; scroll_y_offset = 0
  }
;;

type text_texture_atlas_info =
  { width : int
  ; height : int
  ; bytes : bytes
  }

type font_info =
  { glyph_info_with_char : (char * Freetype.glyph_info_) Array.t
  ; font_size : int
  ; font_texture_atlas : text_texture_atlas_info
  ; font_height : int
  ; ascender : int
  ; descender : int
  }

let get_glyph_info_from_glyph ~glyph ~font_info =
  let opt = Array.find_opt (fun (c', _) -> c' = glyph) font_info.glyph_info_with_char in
  try
    let _, gi = Option.get opt in
    gi
  with
  | Invalid_argument e -> failwith (__FUNCTION__ ^ "; " ^ e)
;;

let get_text_wrap_info ~bbox ~glyph ~x ~y ~font_info =
  if glyph = '\n'
  then ~new_x:bbox.x, ~new_y:(y + font_info.font_height), ~wraps:true
  else (
    let glyph_info = get_glyph_info_from_glyph ~glyph ~font_info in
    if x + glyph_info.x_advance > bbox.x + bbox.width
    then
      ( ~new_x:(bbox.x + glyph_info.x_advance)
      , ~new_y:(y + font_info.font_height)
      , ~wraps:true )
    else ~new_x:(x + glyph_info.x_advance), ~new_y:y, ~wraps:false)
;;

let create_textarea_box () =
  { default_box with
    content = Some (Textarea default_text_area_information)
  ; on_event = Some default_textarea_event_handler
  }
;;

let create_scrollbar ~content =
  let original_mousedown_pos_was_within = ref false in
  let diff_from_initial_mousedown_to_top_of_bar = ref 0 in
  let parent =
    { default_box with
      height_constraint = Some Max
    ; bbox = Some { x = 0; y = 0; width = 15; height = 0 }
    ; background_color = 0.8, 0.8, 0.8, 1.
    ; content = None
    ; horizontal_align = Some Center
    }
  in
  parent.content
  <- Some
       (Box
          { default_box with
            bbox = Some { x = 0; y = 0; width = 8; height = 50 }
          ; background_color = 0., 0., 0., 1.
          ; on_event =
              Some
                (fun ~b ~e ->
                  match e with
                  | Sdl.MouseMotionEvt { x; y; _ } ->
                    (match b with
                     | Some b ->
                       (match !holding_mousedown with
                        | `True (~original_x, ~original_y) ->
                          let { left; right; top; bottom } = get_box_sides ~box:parent in
                          let bbox = Option.get b.bbox in
                          let _, height_ratio =
                            get_logical_to_opengl_window_dims_ratio ()
                          in
                          let y = y * height_ratio in
                          if
                            is_within_box
                              ~box:b
                              ~x:original_x
                              ~y:original_y
                              ~from_sdl_evt:true
                            && not !original_mousedown_pos_was_within
                          then (
                            original_mousedown_pos_was_within := true;
                            diff_from_initial_mousedown_to_top_of_bar := y - bbox.y);
                          if !original_mousedown_pos_was_within
                          then
                            if
                              y - !diff_from_initial_mousedown_to_top_of_bar + bbox.height
                              <= bottom
                              && y - !diff_from_initial_mousedown_to_top_of_bar >= top
                            then
                              b.bbox
                              <- Some
                                   { bbox with
                                     y = y - !diff_from_initial_mousedown_to_top_of_bar
                                   }
                        | `False -> original_mousedown_pos_was_within := false)
                     | None -> ())
                  | _ -> ())
          });
  parent
;;

let create_scrollcontainer ~content =
  let scroll_bar = create_scrollbar ~content in
  ScrollContainer
    { content
    ; scroll = scroll_bar
    ; container =
        { default_box with
          width_constraint = Some Min
        ; height_constraint = Some Min
        ; content = Some (Boxes [ content; scroll_bar ])
        ; flow = Some Horizontal
        ; on_event =
            Some
              (fun ~b ~e ->
                match e with
                | Sdl.MouseWheelEvt { x; y; _ } -> ()
                | _ -> ())
        }
    }
;;

let clone_box ~(box : box) =
  let visited = ref [] in
  let rec clone_box' box =
    if List.exists (fun b -> b == box) !visited
    then failwith "Recursive structure detected"
    else visited := box :: !visited;
    { name = box.name
    ; content =
        (match box.content with
         | Some (Box b) -> Some (Box (clone_box' b))
         | Some (Boxes list) -> Some (Boxes (List.map (fun b -> clone_box' b) list))
         | Some (Text _) | Some (Textarea _) | Some (ScrollContainer _) | None ->
           box.content)
    ; bbox = box.bbox
    ; text_wrap = box.text_wrap
    ; background_color = box.background_color
    ; border = box.border
    ; flow = box.flow
    ; font_size = box.font_size
    ; width_constraint = box.width_constraint
    ; height_constraint = box.height_constraint
    ; clip_content = box.clip_content
    ; position_type = box.position_type
    ; allow_vertical_scroll = box.allow_vertical_scroll
    ; allow_horizontal_scroll = box.allow_horizontal_scroll
    ; horizontal_align = box.horizontal_align
    ; vertical_align = box.vertical_align
    ; on_event = box.on_event
    ; scroll_y_offset = box.scroll_y_offset
    ; scroll_x_offset = box.scroll_x_offset
    }
  in
  clone_box' box
;;

let get_text_texture_atlas_info
      ~(glyph_info_with_char : (char * Freetype.glyph_info_) Array.t)
  =
  let face = Freetype.face in
  let descender = Freetype.get_descender face in
  let ascender = Freetype.get_ascender face in
  let widths_summed =
    Array.fold_left (fun acc (_, gi) -> acc + gi.Freetype.width) 0 glyph_info_with_char
  in
  let global_font_height = ascender - descender in
  let bytes_texture_atlas = Bytes.create (widths_summed * global_font_height) in
  (*
       the font glyph texture atlas is all of the glyphs that is loaded
       concatenated into a large bytes array

       widths of glyphs summed * font height
       ex:
         ABCDEF...
     *)
  let current_width = ref 0 in
  for glyph_info_index = 0 to Array.length glyph_info_with_char - 1 do
    let _, glyph_info = glyph_info_with_char.(glyph_info_index) in
    for row = 0 to glyph_info.rows - 1 do
      let slice = Bytes.sub glyph_info.bytes (row * glyph_info.width) glyph_info.width in
      Bytes.blit
        slice
        0
        bytes_texture_atlas
        (!current_width + (widths_summed * row))
        glyph_info.width
    done;
    current_width := !current_width + glyph_info.width
  done;
  { (* width and height of text texture *)
    width = widths_summed
  ; height = global_font_height
  ; bytes = bytes_texture_atlas
  }
;;

let get_new_font_info_with_font_size ~(font_size : int) ~(face : Freetype.ft_face) =
  let _, height_ratio = get_logical_to_opengl_window_dims_ratio () in
  (* scaling font_size appropriately based on ratio between the opengl window in pixels and the sdl window's pixels *)
  let font_size = font_size * height_ratio in
  let glyph_info_with_char =
    Array.init
      (126 - 32 + 1)
      (fun i -> Freetype.get_ascii_char_glyph_info_ face (i + 32) font_size)
  in
  let font_height = Freetype.get_font_height face in
  let descender = Freetype.get_descender face in
  let ascender = Freetype.get_ascender face in
  let text_texture_atlas_info = get_text_texture_atlas_info ~glyph_info_with_char in
  { glyph_info_with_char
  ; font_size
  ; font_texture_atlas = text_texture_atlas_info
  ; font_height
  ; ascender
  ; descender
  }
;;
