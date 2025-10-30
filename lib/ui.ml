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
  ; mutable scroll_x_offset : int
  ; mutable scroll_y_offset : int
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
      let width_ratio, height_ratio = Sdl.get_logical_to_opengl_window_dims_ratio () in
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
  ; text_wrap = true
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

let get_glyph_info_from_glyph ~glyph ~font_info =
  let opt =
    Array.find_opt (fun (c', _) -> c' = glyph) font_info.Freetype.glyph_info_with_char
  in
  try
    let _, gi = Option.get opt in
    gi
  with
  | Invalid_argument e -> failwith (__FUNCTION__ ^ "; " ^ e)
;;

let get_text_wrap_info ~bbox ~glyph ~x ~y ~font_info =
  if glyph = '\n'
  then ~new_x:bbox.x, ~new_y:(y + font_info.Freetype.font_height), ~wraps:true
  else (
    let glyph_info = get_glyph_info_from_glyph ~glyph ~font_info in
    (* TODO: i need to actually check here if text_wrap is true or not *)
    if x + glyph_info.x_advance > bbox.x + bbox.width
    then
      ( ~new_x:(bbox.x + glyph_info.x_advance)
      , ~new_y:(y + font_info.font_height)
      , ~wraps:true )
    else ~new_x:(x + glyph_info.x_advance), ~new_y:y, ~wraps:false)
;;

module TextTextureInfo = struct
  type texture_info =
    { gl_texture_id : int
    ; font_size : int
    ; font_info : Freetype.font_info
    }

  let text_textures_with_different_font_sizes : texture_info list ref = ref []

  let get_or_add_font_size_text_texture ~(font_size : int) =
    let option =
      List.find_opt
        (fun { font_size = font_size'; _ } -> font_size' = font_size)
        !text_textures_with_different_font_sizes
    in
    match option with
    | Some { font_info; gl_texture_id; _ } -> ~font_info, ~gl_texture_id
    | None ->
      let gl_buffer_glyph_texture_atlas = Opengl.gl_gen_texture () in
      let font_info =
        Freetype.get_new_font_info_with_font_size ~font_size ~face:Freetype.face
      in
      text_textures_with_different_font_sizes
      := { gl_texture_id = gl_buffer_glyph_texture_atlas; font_size; font_info }
         :: !text_textures_with_different_font_sizes;
      Opengl.gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
      Opengl.set_gl_tex_parameters_ui_text ();
      Opengl.gl_teximage_2d
        ~bytes:font_info.font_texture_atlas.bytes
        ~width:font_info.font_texture_atlas.width
        ~height:font_info.font_texture_atlas.height;
      ~font_info, ~gl_texture_id:gl_buffer_glyph_texture_atlas
  ;;
end

let create_textarea_box () =
  { default_box with
    content = Some (Textarea default_text_area_information)
  ; on_event = Some default_textarea_event_handler
  }
;;

let get_text_bounding_box ~(box : box) =
  let rope =
    match box.content with
    | Some (Text s) -> Rope.of_string s
    | Some (Textarea { text; _ }) ->
      (match text with
       | Some r -> r
       | None -> Rope.of_string "")
    | _ -> failwith __FUNCTION__
  in
  let bbox = Option.value box.bbox ~default:default_bbox in
  let min_x, min_y, max_x, max_y =
    ref Int.max_int, ref Int.max_int, ref Int.min_int, ref Int.min_int
  in
  let ~font_info, .. =
    TextTextureInfo.get_or_add_font_size_text_texture ~font_size:Freetype.font_size
  in
  let Rope.{ x; y; _ } =
    Rope.traverse_rope
      ~rope
      ~handle_result:(fun (acc : Rope.rope_traversal_info Rope.traverse_info) c ->
        let (Rope_Traversal_Info acc) = acc in
        match c with
        | '\n' ->
          min_y := min !min_y acc.y;
          max_y := max !max_y acc.y;
          Rope_Traversal_Info
            { y = acc.y + font_info.Freetype.font_height
            ; x = bbox.x
            ; rope_pos = acc.rope_pos + 1
            }
        | _ ->
          min_x := min !min_x acc.x;
          max_x := max !max_x acc.x;
          let gi = get_glyph_info_from_glyph ~glyph:c ~font_info in
          let ~new_x, ~new_y, .. =
            get_text_wrap_info ~bbox ~glyph:c ~x:acc.x ~y:acc.y ~font_info
          in
          Rope_Traversal_Info { y = new_y; x = new_x; rope_pos = acc.rope_pos + 1 })
      ~result:(Rope_Traversal_Info { x = bbox.x; y = bbox.y; rope_pos = 0 })
  in
  min_x := min !min_x x;
  max_x := max !max_x x;
  min_y := min !min_y y;
  max_y := max !max_y y;
  ~min_x:!min_x, ~max_x:!max_x, ~min_y:!min_y, ~max_y:!max_y
;;

let calculate_bounding_box_of_text_content ~(box : box) =
  let bbox =
    match Option.get box.bbox with
    | bbox -> bbox
    | exception Invalid_argument e -> failwith (__FUNCTION__ ^ "; " ^ e)
  in
  match box.content with
  | Some (Text _) | Some (Textarea _) -> get_text_bounding_box ~box
  | _ -> failwith "unreachable"
;;

(* I thought this would be simpler to calculate but sometimes the box's bbox doesn't reflect the potential size that
its contents could take up, so recursion is necessary *)
let rec calculate_content_boundaries ~(box : box) =
  let { left; right; top; bottom } = get_box_sides ~box in
  match box.content with
  | Some (Box b) ->
    let { left = left'; right = right'; top = top'; bottom = bottom' } =
      calculate_content_boundaries ~box:b
    in
    { left = min left left'
    ; right = max right right'
    ; top = min top top'
    ; bottom = max bottom bottom'
    }
  | Some (Boxes list) ->
    let ( min_horizontal_position
        , max_horizontal_position
        , min_vertical_position
        , max_vertical_position )
      =
      List.fold_left
        (fun (min_horizontal, max_horizontal, min_vertical, max_vertical) b ->
           let { left; right; top; bottom } = get_box_sides ~box:b in
           ( min min_horizontal left
           , max max_horizontal right
           , min min_vertical top
           , max max_vertical bottom ))
        (left, right, top, bottom)
        list
    in
    { left = min_horizontal_position
    ; right = max_horizontal_position
    ; top = min_vertical_position
    ; bottom = max_vertical_position
    }
  | Some (Text _) | Some (Textarea _) ->
    let ~min_x, ~max_x, ~min_y, ~max_y = calculate_bounding_box_of_text_content ~box in
    { left = min left min_x
    ; right = max right max_x
    ; top = min top min_y
    ; bottom = max bottom max_y
    }
  | Some (ScrollContainer { container; _ }) -> calculate_content_boundaries ~box:container
  | None -> { left; right; top; bottom }
;;

let scrollbar_event_logic ~parent ~content =
  let original_mousedown_pos_was_within = ref false in
  let diff_from_initial_mousedown_to_top_of_bar = ref 0 in
  fun ~b ~e ->
    match e with
    | Sdl.MouseMotionEvt { x; y; _ } ->
      (match b with
       | Some b ->
         (match !holding_mousedown with
          | `True (~original_x, ~original_y) ->
            let { left; right; top; bottom } = get_box_sides ~box:parent in
            let bbox = Option.get b.bbox in
            let _, height_ratio = Sdl.get_logical_to_opengl_window_dims_ratio () in
            let y = y * height_ratio in
            if
              is_within_box ~box:b ~x:original_x ~y:original_y ~from_sdl_evt:true
              && not !original_mousedown_pos_was_within
            then (
              original_mousedown_pos_was_within := true;
              diff_from_initial_mousedown_to_top_of_bar := y - bbox.y);
            let { bottom = content_bottom; _ } =
              calculate_content_boundaries ~box:content
            in
            let content_height = content_bottom - top
            and parent_height = bottom - top in
            if content_height > parent_height
            then
              Option.iter
                (fun bbox ->
                   b.bbox
                   <- Some
                        { bbox with
                          height = parent_height * parent_height / content_height
                        })
                b.bbox;
            if !original_mousedown_pos_was_within
            then
              Option.iter
                (fun bbox ->
                   content.scroll_y_offset
                   <- -content_height * (bbox.y - top) / parent_height;
                   b.bbox
                   <- Some
                        { bbox with
                          y =
                            max
                              top
                              (min
                                 (bottom - bbox.height)
                                 (y - !diff_from_initial_mousedown_to_top_of_bar))
                        })
                b.bbox
          | `False -> original_mousedown_pos_was_within := false)
       | None -> ())
    | _ -> ()
;;

let create_scrollbar ~parent ~content =
  { default_box with
    bbox = Some { x = 0; y = 0; width = 8; height = 50 }
  ; background_color = 0., 0., 0., 1.
  ; on_event = Some (scrollbar_event_logic ~parent ~content)
  }
;;

let create_scrollbar_container ~content =
  let parent =
    { default_box with
      height_constraint = Some Max
    ; bbox = Some { x = 0; y = 0; width = 15; height = 0 }
    ; background_color = 0.8, 0.8, 0.8, 1.
    ; content = None
    ; horizontal_align = Some Center
    }
  in
  let scrollbar = create_scrollbar ~parent ~content in
  parent.content <- Some (Box scrollbar);
  parent
;;

let create_scrollcontainer ~content =
  let scrollbar_container = create_scrollbar_container ~content in
  ScrollContainer
    { content
    ; scroll = scrollbar_container
    ; container =
        { default_box with
          width_constraint = Some Min
        ; height_constraint = Some Min
        ; content = Some (Boxes [ content; scrollbar_container ])
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
         | Some (ScrollContainer { container; content; scroll }) ->
           let content = clone_box' content in
           Some (create_scrollcontainer ~content)
         | Some (Text _) | Some (Textarea _) | None -> box.content)
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
