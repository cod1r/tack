open Ui_types

let get_constraint_string constraint' =
  match constraint' with
  | Some (Content { fallback_size }) ->
    Printf.sprintf "Content: fallback_size: %d" fallback_size
  | Some (Parent { fallback_size }) ->
    Printf.sprintf "Parent: fallback_size: %d" fallback_size
  | Some (MinContent { min }) -> Printf.sprintf "MinContent: min: %d" min
  | Some (MaxContent { max }) -> Printf.sprintf "MaxContent: max: %d" max
  | Some (MinMaxContent { min; max }) ->
    Printf.sprintf "MinMaxContent: min: %d max: %d" min max
  | Some (MinParent { min }) -> Printf.sprintf "MinParent: min: %d" min
  | Some (MaxParent { max }) -> Printf.sprintf "MaxParent: max: %d" max
  | Some (MinMaxParent { min; max }) ->
    Printf.sprintf "MinMaxParent: min: %d max: %d" min max
  | Some (Number n) -> Printf.sprintf "Number: %d" n
  | Some (ExpandAsMuchPossible { fallback_size }) ->
    Printf.sprintf "ExpandAsMuchPossible {fallback_size: %d}" fallback_size
  | None -> "None"
;;

let print_box ?(depth = 1) box =
  let buffer = Buffer.create 65536 in
  let formatter = Format.formatter_of_buffer buffer in
  let rec print_box' depth' box =
    Format.fprintf
      formatter
      "@[<v 0>{@;\
       <0 1>@[<v 0>name: %s@,\
       update: %s@,\
       bbox: %s@,\
       text_wrap: %b@,\
       background_color: %s@,\
       border: %s@,\
       flow: %s@,\
       font_size: %s@,\
       width_constraint: %s@,\
       height_constraint: %s@,\
       clip_content: %b@,\
       position_type: %s@,\
       allow_horizontal_scroll: %b@,\
       allow_vertical_scroll: %b@,\
       horizontal_align: %s@,\
       vertical_align: %s@,\
       on_event: %s@,\
       scroll_x_offset: %d@,\
       scroll_y_offset: %d@,\
       focusable: %b@,\
       content:@;\
       <0 1>@[<v 0>"
      (Option.value box.name ~default:"None")
      (match box.update with
       | Some _ -> "Some update_fn"
       | None -> "None")
      (match box.bbox with
       | Some bbox ->
         Printf.sprintf
           "(x: %d, y: %d, width: %d, height: %d)"
           bbox.x
           bbox.y
           bbox.width
           bbox.height
       | None -> "None")
      box.text_wrap
      (let r, g, b, a = box.background_color in
       Printf.sprintf "%f %f %f %f" r g b a)
      (match box.border with
       | Some
           { top_thickness
           ; right_thickness
           ; bottom_thickness
           ; left_thickness
           ; top_left_corner_options
           ; top_right_corner_options
           ; bottom_left_corner_options
           ; bottom_right_corner_options
           ; color = r, g, b, a
           } ->
         Printf.sprintf
           "top_thickness: %d\n\
            right_thickness: %d\n\
            bottom_thickness: %d\n\
            left_thickness: %d\n\
            top_left_corner_options:\n\
            vertical_radius: %d\n\
            horizontal_radius: %d\n\
            top_right_corner_options:\n\
            vertical_radius: %d\n\
            horizontal_radius: %d\n\
            bottom_left_corner_options:\n\
            vertical_radius: %d\n\
            horizontal_radius: %d\n\
            bottom_right_corner_options:\n\
            vertical_radius: %d\n\
            horizontal_radius: %d\n\
            color: %f %f %f %f"
           top_thickness
           right_thickness
           bottom_thickness
           left_thickness
           top_left_corner_options.vertical_radius
           top_left_corner_options.horizontal_radius
           top_right_corner_options.vertical_radius
           top_right_corner_options.horizontal_radius
           bottom_left_corner_options.vertical_radius
           bottom_left_corner_options.horizontal_radius
           bottom_right_corner_options.vertical_radius
           bottom_right_corner_options.horizontal_radius
           r
           g
           b
           a
       | None -> "None")
      (match box.flow with
       | Some Horizontal -> "horizontal"
       | Some Vertical -> "vertical"
       | None -> "None")
      (match box.font_size with
       | Some n -> string_of_int n
       | None -> "None")
      (get_constraint_string box.width_constraint)
      (get_constraint_string box.height_constraint)
      box.clip_content
      (match box.position_type with
       | Relative { x; y } -> Printf.sprintf "Relative %d %d" x y
       | Absolute -> "Absolute")
      box.allow_horizontal_scroll
      box.allow_vertical_scroll
      (match box.horizontal_align with
       | Some Left -> "Some Left"
       | Some Right -> "Some Right"
       | Some Center -> "Some Center"
       | None -> "None")
      (match box.vertical_align with
       | Some Top -> "Some Top"
       | Some Bottom -> "Some Bottom"
       | Some Center -> "Some Center"
       | None -> "None")
      (match box.on_event with
       | Some _ -> "Some on_event"
       | None -> "None")
      box.scroll_x_offset
      box.scroll_y_offset
      box.focusable;
    if depth' = depth
    then
      if box.content <> None
      then Format.pp_print_string formatter "..."
      else Format.pp_print_string formatter "None"
    else (
      match box.content with
      | Some (Box b) -> print_box' (depth' + 1) b
      | Some (Boxes list) ->
        List.iteri
          (fun i b ->
             Format.pp_open_vbox formatter 0;
             print_box' (depth' + 1) b;
             Format.pp_close_box formatter ();
             if i < List.length list - 1 then Format.pp_print_cut formatter ())
          list
      | Some (Textarea text_area_information) ->
        Format.pp_open_vbox formatter 0;
        Format.pp_print_string formatter "Text area information:";
        Format.pp_print_break formatter 0 1;
        Format.pp_print_string formatter "text: ";
        Format.pp_print_string
          formatter
          (Option.value text_area_information.text ~default:(Rope.of_string "None")
           |> Rope.to_string);
        Format.pp_print_break formatter 0 1;
        Format.pp_print_string formatter "cursor_pos: ";
        Format.pp_print_string
          formatter
          (match text_area_information.cursor_pos with
           | Some cp -> string_of_int cp
           | None -> "None");
        Format.pp_close_box formatter ()
      | Some (Text s) -> Format.pp_print_string formatter ("Text of " ^ s.string)
      | None -> Format.pp_print_string formatter "None");
    Format.pp_close_box formatter ();
    Format.pp_close_box formatter ();
    Format.pp_print_cut formatter ();
    Format.pp_print_string formatter "}";
    Format.pp_close_box formatter ()
  in
  print_box' 1 box;
  Format.pp_print_flush formatter ();
  buffer
;;

let default_bbox : bounding_box = { width = 0; height = 0; x = 0; y = 0 }

let get_box_sides ~(box : box) : box_sides =
  match box.bbox with
  | Some bbox ->
    let right = bbox.x + bbox.width - 1
    and bottom = bbox.y + bbox.height - 1 in
    { left = bbox.x; top = bbox.y; right; bottom }
  | None -> failwith "calling get_box_sides requires a bbox property of Some"
;;

let default_text_area_information =
  { text = None
  ; cursor_pos = None
  ; highlight_pos = None, None
  ; holding_mousedown_rope_pos = None
  ; history = { undo_list = []; redo_list = [] }
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
  assert (box.bbox <> None);
  let { left; right; top; bottom } = get_box_sides ~box in
  x >= left && x <= right && y <= bottom && y >= top
;;

let default_box =
  { name = None
  ; content = None
  ; bbox = None
  ; text_wrap = true
  ; background_color = 1., 1., 1., 0.
  ; border = None
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
  ; focusable = false
  ; update = None
  }
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

let text_caret_width = 3

(* these functions exist purely because polymorphic comparison is slow *)
let get_max_int x y =
  let comparison = Int.compare x y in
  if comparison = 1 then x else y
;;

let get_min_int x y =
  let max_int = get_max_int x y in
  if x = max_int then y else x
;;

let get_text_bounding_box ~(box : box) =
  let rope, is_textarea =
    match box.content with
    | Some (Text { string; _ }) -> Rope.of_string string, false
    | Some (Textarea { text; _ }) ->
      ( (match text with
         | Some r -> r
         | None -> Rope.of_string "")
      , true )
    | _ -> failwith __FUNCTION__
  in
  assert (box.bbox <> None);
  let bbox = Option.get box.bbox in
  let (min_x, min_y, max_x, max_y) : int ref * int ref * int ref * int ref =
    ref Int.max_int, ref Int.max_int, ref Int.min_int, ref Int.min_int
  in
  let ~font_info, .. =
    TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
  in
  let Rope_types.{ y; _ } =
    Rope.traverse_rope
      ~box
      ~font_info
      ~rope
      ~handle_result:
        (Some
           (fun (Rope_Traversal_Info acc) _c ->
             min_y := get_min_int !min_y acc.y;
             min_x := get_min_int !min_x acc.x;
             max_y := get_max_int !max_y acc.y;
             max_x
             := get_max_int !max_x (acc.x + if is_textarea then text_caret_width else 0)))
      ~result:(Rope_Traversal_Info { x = bbox.x; y = bbox.y; rope_pos = 0 })
  in
  max_y := get_max_int !max_y y;
  max_y := !max_y + font_info.font_height;
  ~min_x:!min_x, ~max_x:!max_x, ~min_y:!min_y, ~max_y:!max_y
;;

let calculate_content_boundaries ~(box : box) =
  let { left; right; top; bottom } =
    try get_box_sides ~box with
    | Failure e -> failwith (e ^ __LOC__)
  in
  match box.content with
  | Some (Box b) ->
    let { left = left'; right = right'; top = top'; bottom = bottom' } =
      try get_box_sides ~box:b with
      | Failure e -> failwith (e ^ __LOC__)
    in
    { left = get_min_int left left'
    ; right = get_max_int right right'
    ; top = get_min_int top top'
    ; bottom = get_max_int bottom bottom'
    }
  | Some (Boxes list) ->
    let ( min_horizontal_position
        , max_horizontal_position
        , min_vertical_position
        , max_vertical_position )
      =
      List.fold_left
        (fun (min_horizontal, max_horizontal, min_vertical, max_vertical) b ->
           let { left; right; top; bottom } =
             try get_box_sides ~box:b with
             | Failure e -> failwith (e ^ __LOC__)
           in
           ( get_min_int min_horizontal left
           , get_max_int max_horizontal right
           , get_min_int min_vertical top
           , get_max_int max_vertical bottom ))
        (left, right, top, bottom)
        list
    in
    { left = min_horizontal_position
    ; right = max_horizontal_position
    ; top = min_vertical_position
    ; bottom = max_vertical_position
    }
  | Some (Text _ | Textarea _) ->
    let ~min_x, ~max_x, ~min_y, ~max_y = get_text_bounding_box ~box in
    { left = get_min_int left min_x
    ; right = get_max_int right max_x
    ; top = get_min_int top min_y
    ; bottom = get_max_int bottom max_y
    }
  | None -> { left; right; top; bottom }
;;

let get_xy_pos_of_text_caret ~text_area_info ~box =
  assert (box.bbox <> None);
  let bbox = Option.get box.bbox in
  if Option.is_none text_area_info.cursor_pos
  then None
  else (
    let ~font_info, .. =
      TextTextureInfo.get_or_add_font_size_text_texture
        ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
    in
    let cursor_pos = Option.get text_area_info.cursor_pos in
    match text_area_info.text with
    | Some rope ->
      let x = ref 0
      and y = ref 0 in
      ignore
        (Rope.traverse_rope
           ~box
           ~font_info
           ~rope
           ~handle_result:
             (Some
                (fun (Rope_types.Rope_Traversal_Info acc) _c ->
                  if acc.rope_pos = cursor_pos
                  then (
                    x := acc.x;
                    y := acc.y)))
           ~result:
             (Rope_types.Rope_Traversal_Info { x = bbox.x; y = bbox.y; rope_pos = 0 }));
      Some (~x:!x, ~y:!y)
    | None -> None)
;;

(*
Scrollbars are adjusted which then causes
changes to the content scroll offsets in Ui_rendering via
change_content_scroll_offsets_based_off_scrollbar
*)
let adjust_scrollbar_according_to_textarea_text_caret
      ~mouse_pos_xy
      ~text_area_info
      ~scrollcontainer_info
      ~content
  =
  let { vertical_scroll_info; horizontal_scroll_info } = scrollcontainer_info in
  let { right; left; top; bottom } = get_box_sides ~box:content in
  let { right = content_right
      ; left = content_left
      ; top = content_top
      ; bottom = content_bottom
      }
    =
    calculate_content_boundaries ~box:content
  in
  let ~font_info, .. =
    TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value content.font_size ~default:Freetype.font_size)
  in
  let potential_xy =
    match mouse_pos_xy with
    | Some (x, y) -> Some (x, y)
    | None ->
      (match get_xy_pos_of_text_caret ~text_area_info ~box:content with
       | Some (~x, ~y) -> Some (x + content.scroll_x_offset, y + content.scroll_y_offset)
       | None -> None)
  in
  match potential_xy with
  | Some (x, y) ->
    (match vertical_scroll_info with
     | Some
         { vertical_scroll = scroll; vertical_scrollbar_container = scrollbar_container }
       ->
       assert (scroll.bbox <> None);
       assert (scrollbar_container.bbox <> None);
       let scrollbar_container_bbox = Option.get scrollbar_container.bbox in
       let bbox = Option.get scroll.bbox in
       let scroll_direction_and_amt =
         (* reason for the 2 * font_info.font_height is because
        sometimes the different between y + font_info.font_height and bottom
        isn't enough for the integer division to not end up being 0 *)
         if y < top
         then y - (top + (2 * font_info.font_height))
         else if y + font_info.font_height > bottom
         then y + (2 * font_info.font_height) - bottom
         else 0
       in
       let offset_amount =
         (bottom - top) * scroll_direction_and_amt / (content_bottom - content_top)
       in
       let sign = Int.compare offset_amount 0 in
       if
         (sign = -1 && bbox.y > scrollbar_container_bbox.y)
         || (sign = 1
             && bbox.y
                < scrollbar_container_bbox.y
                  + scrollbar_container_bbox.height
                  - bbox.height)
       then scroll.bbox <- Some { bbox with y = bbox.y + offset_amount }
     | None -> ());
    (match horizontal_scroll_info with
     | Some
         { horizontal_scroll = scroll
         ; horizontal_scrollbar_container = scrollbar_container
         } ->
       assert (scroll.bbox <> None);
       assert (scrollbar_container.bbox <> None);
       let scrollbar_container_bbox = Option.get scrollbar_container.bbox in
       let bbox = Option.get scroll.bbox in
       let scroll_direction_and_amt =
         if x + text_caret_width > right
         then x + text_caret_width - right
         else if x < left
         then x - text_caret_width - left
         else 0
       in
       let offset_amount =
         (right - left) * scroll_direction_and_amt / (content_right - content_left)
       in
       let sign = Int.compare offset_amount 0 in
       if
         (sign = -1 && bbox.x > scrollbar_container_bbox.x)
         || (sign = 1
             && bbox.x
                < scrollbar_container_bbox.x + scrollbar_container_bbox.width - bbox.width
            )
       then scroll.bbox <- Some { bbox with x = bbox.x + offset_amount }
     | None -> ())
  | None -> ()
;;

(* width, height tuple *)
let amt_subtract_due_to_having_scrollcontainer ~box =
  let scrollcontainer =
    List.find_map
      (fun (box', scrollcontainer) -> if box' == box then Some scrollcontainer else None)
      !Ui_globals.scrollcontainers
  in
  match scrollcontainer with
  | Some { horizontal_scroll_info = Some _; vertical_scroll_info = Some _ } ->
    Ui_globals.scrollbar_container_width, Ui_globals.scrollbar_container_width
  | Some { horizontal_scroll_info = Some _; vertical_scroll_info = None } ->
    0, Ui_globals.scrollbar_container_width
  | Some { horizontal_scroll_info = None; vertical_scroll_info = Some _ } ->
    Ui_globals.scrollbar_container_width, 0
  | _ -> 0, 0
;;

let calculate_string_width ~s ~font_info =
  String.fold_left
    (fun acc c ->
       if c = '\n' || c = '\t'
       then acc
       else (
         let gi = font_info.Freetype.glyph_info_with_char.(Char.code c - 32) in
         acc + gi.Freetype.x_advance))
    0
    s
;;

let get_box_size_if_constraint_is_parent context size_constraint (dimension : dimension) =
  match size_constraint with
  | Parent { fallback_size } ->
    (match context.parent with
     | Some { width_constraint; height_constraint; _ } ->
       (match dimension with
        | Width ->
          (match width_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) ->
             fallback_size
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { width; _ } : bounding_box = Option.get parent.bbox in
             width
           | Some (Number n) -> n
           | None -> fallback_size)
        | Height ->
          (match height_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) ->
             fallback_size
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { height; _ } : bounding_box = Option.get parent.bbox in
             height
           | Some (Number n) -> n
           | None -> fallback_size))
     | None -> fallback_size)
  | MinMaxParent { min = min_size; max = max_size } ->
    (match context.parent with
     | Some { width_constraint; height_constraint; _ } ->
       (match dimension with
        | Width ->
          (match width_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) -> min_size
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { width; _ } : bounding_box = Option.get parent.bbox in
             max min_size width |> min max_size
           | Some (Number n) -> max min_size n |> min max_size
           | None -> max_size)
        | Height ->
          (match height_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) -> min_size
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { height; _ } : bounding_box = Option.get parent.bbox in
             max min_size height |> min max_size
           | Some (Number n) -> max min_size n |> min max_size
           | None -> max_size))
     | None -> max_size)
  | MinParent { min = min' } ->
    (match context.parent with
     | Some { width_constraint; height_constraint; _ } ->
       (match dimension with
        | Width ->
          (match width_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) -> min'
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { width; _ } : bounding_box = Option.get parent.bbox in
             max min' width
           | Some (Number n) -> max min' n
           | None -> min')
        | Height ->
          (match height_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) -> min'
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { height; _ } : bounding_box = Option.get parent.bbox in
             max min' height
           | Some (Number n) -> max min' n
           | None -> min'))
     | None -> min')
  | MaxParent { max = max' } ->
    (match context.parent with
     | Some { width_constraint; height_constraint; _ } ->
       (match dimension with
        | Width ->
          (match width_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) -> max'
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { width; _ } : bounding_box = Option.get parent.bbox in
             min max' width
           | Some (Number n) -> min max' n
           | None -> max')
        | Height ->
          (match height_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) -> max'
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { height; _ } : bounding_box = Option.get parent.bbox in
             min max' height
           | Some (Number n) -> min max' n
           | None -> max'))
     | None -> max')
  | ExpandAsMuchPossible { fallback_size } ->
    (match context.parent with
     | Some { width_constraint; height_constraint; _ } ->
       (match dimension with
        | Width ->
          (match width_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) ->
             fallback_size
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { width; _ } : bounding_box = Option.get parent.bbox in
             width
           | Some (Number n) -> n
           | None -> failwith "parent has no constraint")
        | Height ->
          (match height_constraint with
           | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) ->
             fallback_size
           | Some
               ( Parent _
               | MinParent _
               | MaxParent _
               | MinMaxParent _
               | ExpandAsMuchPossible _ ) ->
             let parent = Option.get context.parent in
             assert (Option.is_some parent.bbox);
             let { height; _ } : bounding_box = Option.get parent.bbox in
             height
           | Some (Number n) -> n
           | None -> failwith "parent has no constraint"))
     | None -> failwith "parent has no constraint")
  | _ -> failwith "should only be called with constraints relating to parent"
;;

let handle_expansion_in_list parent list =
  let fixed_sized_total_width, fixed_sized_total_height =
    List.fold_left
      (fun (width, height) b ->
         ( (match b.width_constraint with
            | Some (Number n) -> width + n
            | _ -> width)
         , match b.height_constraint with
           | Some (Number n) -> height + n
           | _ -> height ))
      (0, 0)
      list
  in
  let num_expands_width, num_expands_height =
    List.fold_left
      (fun (width, height) b ->
         ( (match b.width_constraint with
            | Some (ExpandAsMuchPossible _) -> width + 1
            | _ -> width)
         , match b.height_constraint with
           | Some (ExpandAsMuchPossible _) -> height + 1
           | _ -> height ))
      (0, 0)
      list
  in
  if
    num_expands_width > 0
    &&
    match parent.width_constraint with
    | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) -> true
    | _ -> false
  then
    failwith
      "width; contents of parent is list but contents of list are trying to expand to an \
       unknown size of parent";
  if
    num_expands_height > 0
    &&
    match parent.height_constraint with
    | Some (Content _ | MinContent _ | MaxContent _ | MinMaxContent _) -> true
    | _ -> false
  then
    failwith
      "height; contents of parent is list but contents of list are trying to expand to \
       an unknown size of parent";
  if num_expands_width > 0 || num_expands_height > 0
  then (
    assert (parent.bbox <> None);
    let { width = parent_width; height = parent_height; _ } : bounding_box =
      Option.get parent.bbox
    in
    if num_expands_width > 0
    then (
      if parent_width < fixed_sized_total_width
      then
        failwith
          "WHY THE FUCK IS THE PARENT WIDTH LESS THAN THE TOTAL FIXED SIZE OF LIST \
           CHILDREN";
      let even_spread_amt_width =
        (parent_width - fixed_sized_total_width) / num_expands_width
      in
      List.iter
        (fun b ->
           let b_bbox = Option.value b.bbox ~default:default_bbox in
           match b.width_constraint with
           | Some (ExpandAsMuchPossible _) ->
             b.bbox <- Some { b_bbox with width = even_spread_amt_width }
           | _ -> ())
        list);
    if num_expands_height > 0
    then (
      if parent_height < fixed_sized_total_height
      then
        failwith
          "WHY THE FUCK IS THE PARENT height LESS THAN THE TOTAL FIXED SIZE OF LIST \
           CHILDREN";
      let even_spread_amt_height =
        (parent_height - fixed_sized_total_height) / num_expands_height
      in
      List.iter
        (fun b ->
           let b_bbox = Option.value b.bbox ~default:default_bbox in
           match b.height_constraint with
           | Some (ExpandAsMuchPossible _) ->
             b.bbox <- Some { b_bbox with height = even_spread_amt_height }
           | _ -> ())
        list))
;;

(* first size everything with a fixed constraint (Number n)
then apply expands *)
let rec size_each_box_in_list ~context ~list =
  List.iter
    (fun b ->
       let bbox = Option.value b.bbox ~default:default_bbox in
       (match b.width_constraint with
        | Some (Number n) -> b.bbox <- Some { bbox with width = n }
        | Some (Parent _ | MinParent _ | MaxParent _ | MinMaxParent _) ->
          "constraining a box in a list to the parent is not allowed. perhaps \
           ExpandAsMuchPossible is wanted? or not using Boxes?"
          ^ __LOC__
          |> failwith
        | _ -> ());
       match b.height_constraint with
       | Some (Number n) -> b.bbox <- Some { bbox with height = n }
       | Some (Parent _ | MinParent _ | MaxParent _ | MinMaxParent _) ->
         "constraining a box in a list to the parent is not allowed. perhaps \
          ExpandAsMuchPossible is wanted? or not using Boxes?"
         ^ __LOC__
         |> failwith
       | _ -> ())
    list;
  match context.parent with
  | Some parent -> handle_expansion_in_list parent list
  | None -> ()

and constrain_width_height ?(in_list = false) ~(box : box) ~context () =
  let amt_sub_width, amt_sub_height = amt_subtract_due_to_having_scrollcontainer ~box in
  (* the reason parent is considered first is so that I don't have to do some weird recursion
    whenever the child tries to get the parent's size but the parent's size is dependent on
    the parent's parent's size etc *)
  (match box.width_constraint with
   | Some ((Parent _ | MinMaxParent _ | MinParent _ | MaxParent _) as constraint')
     when not in_list ->
     let new_width = get_box_size_if_constraint_is_parent context constraint' Width in
     let bbox = Option.value box.bbox ~default:default_bbox in
     box.bbox <- Some { bbox with width = new_width - amt_sub_width }
   | Some (Number n) ->
     let bbox = Option.value box.bbox ~default:default_bbox in
     box.bbox <- Some { bbox with width = n - amt_sub_width }
   | _ -> ());
  (match box.height_constraint with
   | Some ((Parent _ | MinMaxParent _ | MinParent _ | MaxParent _) as constraint')
     when not in_list ->
     let new_height = get_box_size_if_constraint_is_parent context constraint' Height in
     let bbox = Option.value box.bbox ~default:default_bbox in
     box.bbox <- Some { bbox with height = new_height - amt_sub_height }
   | Some (Number n) ->
     let bbox = Option.value box.bbox ~default:default_bbox in
     box.bbox <- Some { bbox with height = n - amt_sub_height }
   | _ -> ());
  match box.content with
  | Some (Box b) ->
    constrain_width_height ~box:b ~context:{ context with parent = Some box } ();
    assert (b.bbox <> None);
    let b_bbox = Option.get b.bbox in
    if not in_list
    then (
      let bbox = Option.value box.bbox ~default:default_bbox in
      (match box.width_constraint with
       | Some (Content _) ->
         box.bbox <- Some { bbox with width = b_bbox.width - amt_sub_width }
       | Some (MinMaxContent { min = min'; max = max' }) ->
         box.bbox
         <- Some { bbox with width = (max min' b_bbox.width |> min max') - amt_sub_width }
       | Some (MaxContent { max = max' }) ->
         box.bbox <- Some { bbox with width = min max' b_bbox.width - amt_sub_width }
       | Some (MinContent { min = min' }) ->
         box.bbox <- Some { bbox with width = max min' b_bbox.width - amt_sub_width }
       | Some (Number n) -> box.bbox <- Some { bbox with width = n - amt_sub_width }
       | _ -> ());
      let bbox = Option.value box.bbox ~default:default_bbox in
      match box.height_constraint with
      | Some (Content _) ->
        box.bbox <- Some { bbox with width = b_bbox.height - amt_sub_height }
      | Some (MinMaxContent { min = min'; max = max' }) ->
        box.bbox
        <- Some
             { bbox with height = (max min' b_bbox.height |> min max') - amt_sub_height }
      | Some (MaxContent { max = max' }) ->
        box.bbox <- Some { bbox with height = min max' b_bbox.height - amt_sub_height }
      | Some (MinContent { min = min' }) ->
        box.bbox <- Some { bbox with height = max min' b_bbox.height - amt_sub_height }
      | Some (Number n) -> box.bbox <- Some { bbox with height = n - amt_sub_height }
      | _ -> ())
  | Some (Boxes list) ->
    (* what do i do here? list of boxes where each box needs to know about other boxes so i
      need to size things here but also size each box's children recursively and avoid sizing the box
      again in each call... *)
    size_each_box_in_list ~context:{ context with parent = Some box } ~list;
    List.iter
      (fun b ->
         constrain_width_height
           ~in_list:true
           ~box:b
           ~context:{ context with parent = Some box }
           ())
      list;
    (match box.flow with
     | Some Horizontal ->
       let max_height =
         List.fold_left
           (fun acc b ->
              let b_bbox = Option.value b.bbox ~default:default_bbox in
              max b_bbox.height acc)
           0
           list
       in
       let summed_width =
         List.fold_left
           (fun acc b -> acc + (Option.value b.bbox ~default:default_bbox).width)
           0
           list
       in
       let bbox = Option.value box.bbox ~default:default_bbox in
       (match box.width_constraint with
        | Some (Content _) ->
          box.bbox <- Some { bbox with width = summed_width - amt_sub_width }
        | Some (MinContent { min = min' }) ->
          box.bbox <- Some { bbox with width = max min' summed_width - amt_sub_width }
        | Some (MaxContent { max = max' }) ->
          box.bbox <- Some { bbox with width = min max' summed_width - amt_sub_width }
        | Some (MinMaxContent { min = min'; max = max' }) ->
          box.bbox
          <- Some
               { bbox with width = (max min' summed_width |> min max') - amt_sub_width }
        | Some (Number n) -> box.bbox <- Some { bbox with width = n - amt_sub_width }
        | _ -> ());
       let bbox = Option.value box.bbox ~default:default_bbox in
       (match box.height_constraint with
        | Some (Content _) ->
          box.bbox <- Some { bbox with height = max_height - amt_sub_height }
        | Some (MinContent { min = min' }) ->
          box.bbox <- Some { bbox with height = max min' max_height - amt_sub_height }
        | Some (MaxContent { max = max' }) ->
          box.bbox <- Some { bbox with height = min max' max_height - amt_sub_height }
        | Some (MinMaxContent { min = min'; max = max' }) ->
          box.bbox
          <- Some
               { bbox with height = (max min' max_height |> min max') - amt_sub_height }
        | Some (Number n) -> box.bbox <- Some { bbox with height = n - amt_sub_height }
        | _ -> ())
     | Some Vertical ->
       let max_width =
         List.fold_left
           (fun acc b ->
              let b_bbox = Option.value b.bbox ~default:default_bbox in
              max b_bbox.width acc)
           0
           list
       in
       let summed_height =
         List.fold_left
           (fun acc b -> acc + (Option.value b.bbox ~default:default_bbox).height)
           0
           list
       in
       let bbox = Option.value box.bbox ~default:default_bbox in
       (match box.width_constraint with
        | Some (Content _) ->
          box.bbox <- Some { bbox with width = max_width - amt_sub_width }
        | Some (MinContent { min = min' }) ->
          box.bbox <- Some { bbox with width = max min' max_width - amt_sub_width }
        | Some (MaxContent { max = max' }) ->
          box.bbox <- Some { bbox with width = min max' max_width - amt_sub_width }
        | Some (MinMaxContent { min = min'; max = max' }) ->
          box.bbox
          <- Some { bbox with width = (max min' max_width |> min max') - amt_sub_width }
        | Some (Number n) -> box.bbox <- Some { bbox with width = n - amt_sub_width }
        | _ -> ());
       let bbox = Option.value box.bbox ~default:default_bbox in
       (match box.height_constraint with
        | Some (Content _) ->
          box.bbox <- Some { bbox with height = summed_height - amt_sub_height }
        | Some (MinContent { min = min' }) ->
          box.bbox <- Some { bbox with height = max min' summed_height - amt_sub_height }
        | Some (MaxContent { max = max' }) ->
          box.bbox <- Some { bbox with height = min max' summed_height - amt_sub_height }
        | Some (MinMaxContent { min = min'; max = max' }) ->
          box.bbox
          <- Some
               { bbox with
                 height = (max min' summed_height |> min max') - amt_sub_height
               }
        | Some (Number n) -> box.bbox <- Some { bbox with height = n - amt_sub_height }
        | _ -> ())
     | None -> ())
  | Some (Text { string }) ->
    let ~font_info, .. =
      TextTextureInfo.get_or_add_font_size_text_texture
        ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
    in
    let string_width = calculate_string_width ~s:string ~font_info in
    let bbox = Option.value box.bbox ~default:default_bbox in
    (match box.width_constraint with
     | Some (Content _) ->
       box.bbox <- Some { bbox with width = string_width - amt_sub_width }
     | Some (MinContent { min = min' }) ->
       box.bbox <- Some { bbox with width = max min' string_width - amt_sub_width }
     | Some (MaxContent { max = max' }) ->
       box.bbox <- Some { bbox with width = min max' string_width - amt_sub_width }
     | Some (MinMaxContent { min = min'; max = max' }) ->
       box.bbox
       <- Some { bbox with width = (max min' string_width |> min max') - amt_sub_width }
     | _ -> ());
    let bbox = Option.value box.bbox ~default:default_bbox in
    (match box.height_constraint with
     | Some (Content _) ->
       box.bbox <- Some { bbox with height = font_info.font_height - amt_sub_height }
     | Some (MinContent { min = min' }) ->
       box.bbox
       <- Some { bbox with height = max min' font_info.font_height - amt_sub_height }
     | Some (MaxContent { max = max' }) ->
       box.bbox
       <- Some { bbox with height = min max' font_info.font_height - amt_sub_height }
     | Some (MinMaxContent { min = min'; max = max' }) ->
       box.bbox
       <- Some
            { bbox with
              height = (max min' font_info.font_height |> min max') - amt_sub_height
            }
     | _ -> ())
  | Some (Textarea _) ->
    if Option.is_none box.bbox
    then (
      match context.parent with
      | Some parent ->
        (match parent.bbox with
         | Some bbox -> box.bbox <- Some { default_bbox with x = bbox.x; y = bbox.y }
         | None -> box.bbox <- Some default_bbox)
      | None -> box.bbox <- Some default_bbox);
    let ~min_x, ~max_x, ~min_y, ~max_y = get_text_bounding_box ~box in
    let textarea_width = max_x - min_x
    and textarea_height = max_y - min_y in
    let bbox = Option.value box.bbox ~default:default_bbox in
    (match box.width_constraint with
     | Some (Content _) ->
       box.bbox <- Some { bbox with width = textarea_width - amt_sub_width }
     | Some (MinContent { min = min' }) ->
       box.bbox <- Some { bbox with width = max min' textarea_width - amt_sub_width }
     | Some (MaxContent { max = max' }) ->
       box.bbox <- Some { bbox with width = min max' textarea_width - amt_sub_width }
     | Some (MinMaxContent { min = min'; max = max' }) ->
       box.bbox
       <- Some { bbox with width = (max min' textarea_width |> min max') - amt_sub_width }
     | Some (Number n) -> box.bbox <- Some { bbox with width = n - amt_sub_width }
     | _ -> ());
    let bbox = Option.value box.bbox ~default:default_bbox in
    (match box.height_constraint with
     | Some (Content _) ->
       box.bbox <- Some { bbox with height = textarea_height - amt_sub_height }
     | Some (MinContent { min = min' }) ->
       box.bbox <- Some { bbox with height = max min' textarea_height - amt_sub_height }
     | Some (MaxContent { max = max' }) ->
       box.bbox <- Some { bbox with height = min max' textarea_height - amt_sub_height }
     | Some (MinMaxContent { min = min'; max = max' }) ->
       box.bbox
       <- Some
            { bbox with height = (max min' textarea_height |> min max') - amt_sub_height }
     | Some (Number n) -> box.bbox <- Some { bbox with height = n - amt_sub_height }
     | _ -> ())
  | None ->
    (match box.width_constraint with
     | Some (Number n) ->
       let bbox = Option.value box.bbox ~default:default_bbox in
       box.bbox <- Some { bbox with width = n - amt_sub_width }
     | Some (Parent _ | MinParent _ | MaxParent _ | MinMaxParent _) -> ()
     | _ -> ());
    (match box.width_constraint with
     | Some (Number n) ->
       let bbox = Option.value box.bbox ~default:default_bbox in
       box.bbox <- Some { bbox with width = n - amt_sub_height }
     | Some (Parent _ | MinParent _ | MaxParent _ | MinMaxParent _) -> ()
     | _ -> ())
;;

(* I'm not sure how to handle cases where the contents are positioned outside of
   the container. Originally I thought that having elements/boxes being absolutely
   positioned would be fine but that leaves problems like child contents being outside of
   the parent container which poses the question of, what should the min width/height be?
   Perhaps, restricting this functionality when child elements are only positioned relatively
   - absolutely positioned boxes will need to have their sizes specified *)
