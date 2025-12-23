open Ui_types

let focused_element : box option ref = ref None

let scrollbar_container_width = 15

let set_focused_element ~(box : box) = focused_element := Some box

let unfocus_element () = focused_element := None

let default_bbox : bounding_box = {width= 0; height= 0; x= 0; y= 0}

let scrollcontainers : box_content list ref = ref []

let holding_mousedown : [`True of original_x:int * original_y:int | `False] ref
    =
  ref `False

let holding_ctrl = ref false

let get_box_sides ~(box : box) : box_sides =
  match box.bbox with
  | Some bbox ->
      let right = bbox.x + bbox.width and bottom = bbox.y + bbox.height in
      {left= bbox.x; top= bbox.y; right; bottom}
  | None ->
      failwith "calling get_box_sides requires a bbox property of Some"

let default_text_area_information =
  { text= None
  ; cursor_pos= None
  ; highlight_pos= (None, None)
  ; holding_mousedown_rope_pos= None }

let is_within_box ~x ~y ~box ~from_sdl_evt =
  let x, y =
    if from_sdl_evt then
      let width_ratio, height_ratio =
        Sdl.get_logical_to_opengl_window_dims_ratio ()
      in
      (x * width_ratio, y * height_ratio)
    else (x, y)
  in
  let {left; right; top; bottom} =
    try get_box_sides ~box with Failure e -> failwith (e ^ __LOC__)
  in
  x >= left && x <= right && y <= bottom && y >= top

let default_textarea_event_handler =
 fun ~(b : box option) ~e ->
  match b with
  | Some b -> (
    match b.content with
    | Some (Textarea _) -> (
      match e with
      | Sdl.MouseButtonEvt {x; y; _} -> (
        match b.bbox with
        | Some _ ->
            if is_within_box ~x ~y ~from_sdl_evt:true ~box:b && b.focusable then
              set_focused_element ~box:b
        | None ->
            () )
      | _ ->
          () )
    | _ ->
        () )
  | _ ->
      ()

let default_box =
  { name= None
  ; content= None
  ; bbox= None
  ; text_wrap= true
  ; background_color= (1., 1., 1., 0.)
  ; border= false
  ; font_size= None
  ; width_constraint= None
  ; height_constraint= None
  ; clip_content= false
  ; position_type= Relative {x= 0; y= 0}
  ; allow_horizontal_scroll= false
  ; allow_vertical_scroll= false
  ; horizontal_align= None
  ; vertical_align= None
  ; flow= None
  ; on_event= None
  ; scroll_x_offset= 0
  ; scroll_y_offset= 0
  ; focusable= false }

module TextTextureInfo = struct
  type texture_info =
    {gl_texture_id: int; font_size: int; font_info: Freetype.font_info}

  let text_textures_with_different_font_sizes : texture_info list ref = ref []

  let get_or_add_font_size_text_texture ~(font_size : int) =
    let option =
      List.find_opt
        (fun {font_size= font_size'; _} -> font_size' = font_size)
        !text_textures_with_different_font_sizes
    in
    match option with
    | Some {font_info; gl_texture_id; _} ->
        (~font_info, ~gl_texture_id)
    | None ->
        let gl_buffer_glyph_texture_atlas = Opengl.gl_gen_texture () in
        let font_info =
          Freetype.get_new_font_info_with_font_size ~font_size
            ~face:Freetype.face
        in
        text_textures_with_different_font_sizes :=
          {gl_texture_id= gl_buffer_glyph_texture_atlas; font_size; font_info}
          :: !text_textures_with_different_font_sizes ;
        Opengl.gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas ;
        Opengl.set_gl_tex_parameters_ui_text () ;
        Opengl.gl_teximage_2d ~bytes:font_info.font_texture_atlas.bytes
          ~width:font_info.font_texture_atlas.width
          ~height:font_info.font_texture_atlas.height ;
        (~font_info, ~gl_texture_id:gl_buffer_glyph_texture_atlas)
end

let create_textarea_box ?(text : Rope_types.rope option) () =
  { default_box with
    focusable= true
  ; clip_content= true
  ; content=
      Some
        (Textarea
           { default_text_area_information with
             text= (match text with Some text -> Some text | None -> None) } )
  ; allow_vertical_scroll= true
  ; allow_horizontal_scroll= true
  ; on_event= Some default_textarea_event_handler }

let text_caret_width = 3

(* these functions exist purely because polymorphic comparison is slow *)
let get_max_int x y =
  let comparison = Int.compare x y in
  if comparison = 1 then x else y

let get_min_int x y =
  let max_int = get_max_int x y in
  if x = max_int then y else x

let get_text_bounding_box ~(box : box) =
  let rope, is_textarea =
    match box.content with
    | Some (Text {string; _}) ->
        (Rope.of_string string, false)
    | Some (Textarea {text; _}) ->
        ((match text with Some r -> r | None -> Rope.of_string ""), true)
    | _ ->
        failwith __FUNCTION__
  in
  assert (box.bbox <> None) ;
  let bbox = Option.get box.bbox in
  let (min_x, min_y, max_x, max_y) : int ref * int ref * int ref * int ref =
    ( ref (bbox.x + box.scroll_x_offset)
    , ref (bbox.y + box.scroll_y_offset)
    , ref Int.min_int
    , ref Int.min_int )
  in
  let ~font_info, .. =
    TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
  in
  let Rope_types.{y; _} =
    Rope.traverse_rope ~box ~font_info ~rope
      ~handle_result:
        (Some
           (fun (Rope_Traversal_Info acc) _c ->
             max_y := get_max_int !max_y acc.y ;
             max_x :=
               get_max_int !max_x
                 (acc.x + if is_textarea then text_caret_width else 0) ) )
      ~result:
        (Rope_Traversal_Info
           { x= bbox.x + box.scroll_x_offset
           ; y= bbox.y + box.scroll_y_offset
           ; rope_pos= 0 } )
  in
  max_y := get_max_int !max_y y ;
  max_y := !max_y + font_info.font_height ;
  (~min_x:!min_x, ~max_x:!max_x, ~min_y:!min_y, ~max_y:!max_y)

let calculate_content_boundaries ~(box : box) =
  let {left; right; top; bottom} =
    try get_box_sides ~box with Failure e -> failwith (e ^ __LOC__)
  in
  match box.content with
  | Some (Box b) ->
      let {left= left'; right= right'; top= top'; bottom= bottom'} =
        try get_box_sides ~box:b with Failure e -> failwith (e ^ __LOC__)
      in
      { left= get_min_int left left'
      ; right= get_max_int right right'
      ; top= get_min_int top top'
      ; bottom= get_max_int bottom bottom' }
  | Some (Boxes list) ->
      let ( min_horizontal_position
          , max_horizontal_position
          , min_vertical_position
          , max_vertical_position ) =
        List.fold_left
          (fun (min_horizontal, max_horizontal, min_vertical, max_vertical) b ->
            let {left; right; top; bottom} =
              try get_box_sides ~box:b with Failure e -> failwith (e ^ __LOC__)
            in
            ( get_min_int min_horizontal left
            , get_max_int max_horizontal right
            , get_min_int min_vertical top
            , get_max_int max_vertical bottom ) )
          (left, right, top, bottom) list
      in
      { left= min_horizontal_position
      ; right= max_horizontal_position
      ; top= min_vertical_position
      ; bottom= max_vertical_position }
  | Some (Text _ | Textarea _) ->
      let ~min_x, ~max_x, ~min_y, ~max_y = get_text_bounding_box ~box in
      { left= get_min_int left min_x
      ; right= get_max_int right max_x
      ; top= get_min_int top min_y
      ; bottom= get_max_int bottom max_y }
  | Some (ScrollContainer {container; _}) ->
      let {left= left'; right= right'; top= top'; bottom= bottom'} =
        get_box_sides ~box:container
      in
      { left= get_min_int left left'
      ; right= get_max_int right right'
      ; top= get_min_int top top'
      ; bottom= get_max_int bottom bottom' }
  | Some (TextAreaWithLineNumbers {container; _}) ->
      let {left= left'; right= right'; top= top'; bottom= bottom'} =
        get_box_sides ~box:container
      in
      { left= get_min_int left left'
      ; right= get_max_int right right'
      ; top= get_min_int top top'
      ; bottom= get_max_int bottom bottom' }
  | None ->
      {left; right; top; bottom}

let get_xy_pos_of_text_caret ~text_area_info ~box =
  assert (box.bbox <> None) ;
  let bbox = Option.get box.bbox in
  if Option.is_none text_area_info.cursor_pos then None
  else
    let ~font_info, .. =
      TextTextureInfo.get_or_add_font_size_text_texture
        ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
    in
    let cursor_pos = Option.get text_area_info.cursor_pos in
    match text_area_info.text with
    | Some rope ->
        let start_x = bbox.x + box.scroll_x_offset
        and start_y = bbox.y + box.scroll_y_offset
        and x = ref 0
        and y = ref 0 in
        ignore
          (Rope.traverse_rope ~box ~font_info ~rope
             ~handle_result:
               (Some
                  (fun (Rope_types.Rope_Traversal_Info acc) _c ->
                    if acc.rope_pos = cursor_pos then (
                      x := acc.x ;
                      y := acc.y ) ) )
             ~result:
               (Rope_types.Rope_Traversal_Info
                  {x= start_x; y= start_y; rope_pos= 0} ) ) ;
        Some (~x:!x, ~y:!y)
    | None ->
        None

let adjust_scrollbar_according_to_textarea_text_caret ~text_area_info ~scroll
    ~orientation ~content =
  let {right; left; top; bottom} = get_box_sides ~box:content in
  let { right= content_right
      ; left= content_left
      ; top= content_top
      ; bottom= content_bottom } =
    calculate_content_boundaries ~box:content
  in
  let ~font_info, .. =
    TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value content.font_size ~default:Freetype.font_size)
  in
  match get_xy_pos_of_text_caret ~text_area_info ~box:content with
  | Some (~x, ~y) -> begin
    match scroll.bbox with
    | Some bbox -> begin
      match orientation with
      | Horizontal ->
          if x + text_caret_width > right then begin
            scroll.bbox <-
              Some
                { bbox with
                  x=
                    bbox.x
                    + (right - left)
                      * (x + text_caret_width - right)
                      / (content_right - content_left) }
          end ;
          if x < left then begin
            scroll.bbox <-
              Some
                { bbox with
                  x=
                    bbox.x
                    + (right - left)
                      * (x - text_caret_width - left)
                      / (content_right - content_left) }
          end
      | Vertical ->
          if y < top then begin
            scroll.bbox <-
              Some
                { bbox with
                  y=
                    bbox.y
                    + (bottom - top)
                      * (y - (top + font_info.font_height))
                      / (content_bottom - content_top) }
          end ;
          if y + font_info.font_height > bottom then begin
            scroll.bbox <-
              Some
                { bbox with
                  y=
                    bbox.y
                    + (bottom - top)
                      * ( y + font_info.font_height
                        - (bottom - font_info.font_height) )
                      / (content_bottom - content_top) }
          end
      end
    | None ->
        failwith ("SHOULD HAVE BBOX FOR scroll.bbox " ^ __LOC__)
    end
  | None ->
      ()

let get_available_size_for_maxed_constrained_inner_boxes
    ~(fixed_sized_boxes : box list) ~(parent_bbox : bounding_box)
    ~(measurement : [`Width | `Height]) ~number_of_constrained =
  let summed_fixed, parent_measurement =
    match measurement with
    | `Width ->
        ( List.fold_left
            (fun acc b ->
              (Option.value b.bbox ~default:default_bbox).width + acc )
            0 fixed_sized_boxes
        , parent_bbox.width )
    | `Height ->
        ( List.fold_left
            (fun acc b ->
              (Option.value b.bbox ~default:default_bbox).height + acc )
            0 fixed_sized_boxes
        , parent_bbox.height )
  in
  let left_over = get_max_int 0 (parent_measurement - summed_fixed) in
  left_over / number_of_constrained

let handle_maximizing_of_inner_content_size ~(parent_box : box) =
  assert (parent_box.bbox <> None) ;
  let parent_bbox = Option.value parent_box.bbox ~default:default_bbox in
  match parent_box.content with
  | Some (Box b) -> (
      begin match b.width_constraint with
      | Some Max ->
          let b_bbox = Option.value b.bbox ~default:default_bbox in
          b.bbox <- Some {b_bbox with width= parent_bbox.width}
      | Some Min | None ->
          ()
      end ;
      match b.height_constraint with
      | Some Max ->
          let b_bbox = Option.value b.bbox ~default:default_bbox in
          b.bbox <- Some {b_bbox with height= parent_bbox.height}
      | Some Min | None ->
          () )
  | Some (Boxes list) -> (
      let fixed_width_boxes =
        List.filter (fun b -> Option.is_none b.width_constraint) list
      in
      let fixed_height_boxes =
        List.filter (fun b -> Option.is_none b.height_constraint) list
      in
      let constrained_width_boxes =
        List.filter (fun b -> b.width_constraint = Some Max) list
      in
      let constrained_height_boxes =
        List.filter (fun b -> b.height_constraint = Some Max) list
      in
      match parent_box.flow with
      | Some Horizontal ->
          ( if List.length constrained_width_boxes > 0 then
              let width_for_each_constrained_box =
                get_available_size_for_maxed_constrained_inner_boxes
                  ~fixed_sized_boxes:fixed_width_boxes ~parent_bbox
                  ~measurement:`Width
                  ~number_of_constrained:(List.length constrained_width_boxes)
              in
              List.iter
                (fun b ->
                  let bbox = Option.value b.bbox ~default:default_bbox in
                  b.bbox <-
                    Some {bbox with width= width_for_each_constrained_box} )
                constrained_width_boxes ) ;
          List.iter
            (fun b ->
              let bbox = Option.value b.bbox ~default:default_bbox in
              b.bbox <- Some {bbox with height= parent_bbox.height} )
            constrained_height_boxes
      | Some Vertical ->
          ( if List.length constrained_height_boxes > 0 then
              let height_for_each_constrained_box =
                get_available_size_for_maxed_constrained_inner_boxes
                  ~fixed_sized_boxes:fixed_height_boxes ~parent_bbox
                  ~measurement:`Height
                  ~number_of_constrained:(List.length constrained_height_boxes)
              in
              List.iter
                (fun b ->
                  let bbox = Option.value b.bbox ~default:default_bbox in
                  b.bbox <-
                    Some {bbox with height= height_for_each_constrained_box} )
                constrained_height_boxes ) ;
          List.iter
            (fun b ->
              let bbox = Option.value b.bbox ~default:default_bbox in
              b.bbox <- Some {bbox with width= parent_bbox.width} )
            constrained_width_boxes
      | _ ->
          () )
  | Some (Text _) ->
      ()
  | Some (Textarea _) ->
      ()
  | Some (ScrollContainer _) ->
      ()
  | Some (TextAreaWithLineNumbers {container; _}) ->
      assert (parent_box.bbox <> None) ;
      container.bbox <- parent_box.bbox
  | None ->
      ()

let calculate_string_width ~s ~font_info =
  String.fold_left
    (fun acc c ->
      if c = '\n' || c = '\t' then acc
      else
        let gi = font_info.Freetype.glyph_info_with_char.(Char.code c - 32) in
        acc + gi.Freetype.x_advance )
    0 s

let rec clamp_width_or_height_to_content_size ~(box : box)
    ~(measurement : [`Width | `Height]) ~context =
  assert (Option.is_some box.bbox) ;
  let bbox = Option.get box.bbox in
  match box.content with
  | Some (Box b) -> (
      constrain_width_height ~box:b ~context:{context with parent= Some box} ;
      assert (Option.is_some b.bbox) ;
      let inner_bbox = Option.get b.bbox in
      match measurement with
      | `Width when box.width_constraint = Some Min ->
          box.bbox <- Some {bbox with width= inner_bbox.width}
      | `Height when box.height_constraint = Some Min ->
          box.bbox <- Some {bbox with height= inner_bbox.height}
      | _ ->
          () )
  | Some (Boxes list) -> (
      List.iter
        (fun b ->
          constrain_width_height ~box:b ~context:{context with parent= Some box} )
        list ;
      match box.flow with
      | Some Vertical -> (
          let summed_size =
            List.fold_left
              (fun acc b ->
                acc + (Option.value b.bbox ~default:default_bbox).height )
              0 list
          in
          let max_width =
            List.fold_left
              (fun acc b ->
                get_max_int acc
                  (Option.value b.bbox ~default:default_bbox).width )
              0 list
          in
          match measurement with
          | `Width when box.width_constraint = Some Min ->
              box.bbox <- Some {bbox with width= max_width}
          | `Height when box.height_constraint = Some Min ->
              box.bbox <- Some {bbox with height= summed_size}
          | _ ->
              () )
      | Some Horizontal -> (
          let summed_size =
            List.fold_left
              (fun acc b ->
                acc + (Option.value b.bbox ~default:default_bbox).width )
              0 list
          in
          let max_height =
            List.fold_left
              (fun acc b ->
                let b_height =
                  (Option.value b.bbox ~default:default_bbox).height
                in
                get_max_int acc b_height )
              0 list
          in
          match measurement with
          | `Width when box.width_constraint = Some Min ->
              box.bbox <- Some {bbox with width= summed_size}
          | `Height when box.height_constraint = Some Min ->
              box.bbox <- Some {bbox with height= max_height}
          | _ ->
              () )
      | None ->
          () )
  | Some (Text {string}) -> (
      let ~font_info, .. =
        TextTextureInfo.get_or_add_font_size_text_texture
          ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
      in
      let string_width = calculate_string_width ~s:string ~font_info in
      (* this doesn't handle the case of text wrapping *)
      match measurement with
      | `Width when box.width_constraint = Some Min ->
          box.bbox <- Some {bbox with width= string_width}
      | `Height when box.width_constraint = Some Min ->
          box.bbox <- Some {bbox with height= font_info.font_height}
      | _ ->
          () )
  | Some (Textarea _)
    when box.width_constraint = Some Min || box.height_constraint = Some Min ->
      let {left; right; top; bottom} = calculate_content_boundaries ~box in
      box.bbox <-
        Some {x= left; y= top; width= right - left; height= bottom - top}
  | Some (ScrollContainer {container; scrollbar_container; orientation; _}) -> (
    match measurement with
    | `Width when box.width_constraint = Some Min -> (
      match orientation with
      | Vertical ->
          let {width= content_width; _} : bounding_box =
            Option.value container.bbox ~default:default_bbox
          and {width= scrollcontainer_width; _} : bounding_box =
            Option.value scrollbar_container.bbox ~default:default_bbox
          in
          box.bbox <-
            Some {bbox with width= scrollcontainer_width + content_width}
      | Horizontal ->
          let {width= content_width; _} : bounding_box =
            Option.value container.bbox ~default:default_bbox
          in
          box.bbox <- Some {bbox with width= content_width} )
    | `Height when box.height_constraint = Some Min -> (
      match orientation with
      | Horizontal ->
          let {height= content_height; _} : bounding_box =
            Option.value container.bbox ~default:default_bbox
          and {height= scrollcontainer_height; _} : bounding_box =
            Option.value scrollbar_container.bbox ~default:default_bbox
          in
          box.bbox <-
            Some {bbox with height= scrollcontainer_height + content_height}
      | Vertical ->
          let {height= content_height; _} : bounding_box =
            Option.value container.bbox ~default:default_bbox
          in
          box.bbox <- Some {bbox with height= content_height} )
    | _ ->
        () )
  | Some (TextAreaWithLineNumbers {container; _}) -> begin
      constrain_width_height ~box:container
        ~context:{context with parent= Some box} ;
      assert (container.bbox <> None) ;
      match measurement with
      | `Width when box.width_constraint = Some Min ->
          box.bbox <- container.bbox
      | `Height when box.height_constraint = Some Min ->
          box.bbox <- container.bbox
      | _ ->
          ()
      end
  | None ->
      ()
  | _ ->
      ()

and calculate_box_position ~(box : Ui_types.box)
    ~(context : Ui_types.ui_traversal_context) =
  (* box position should always start inside the parent if there's no bbox defined before hand *)
  begin match box.bbox with
  | Some _ ->
      ()
  | None -> (
    match context.parent with
    | Some parent ->
        assert (Option.is_some parent.bbox) ;
        let parent_bbox = Option.get parent.bbox in
        box.bbox <-
          Some {x= parent_bbox.x; y= parent_bbox.y; width= 0; height= 0}
    | None ->
        failwith "WHY IS THERE NO BBOX WITH NO PARENT" )
  end ;
  match box.position_type with
  | Relative {x; y} ->
      assert (Option.is_some box.bbox) ;
      let bbox = Option.get box.bbox in
      box.bbox <- Some {bbox with x= bbox.x + x; y= bbox.y + y}
  | Absolute ->
      ()

(* I'm not sure how to handle cases where the contents are positioned outside of
   the container. Originally I thought that having elements/boxes being absolutely
   positioned would be fine but that leaves problems like child contents being outside of
   the parent container which poses the question of, what should the min width/height be?
   Perhaps, restricting this functionality when child elements are only positioned relatively *)
and constrain_width_height ~(box : box) ~context =
  calculate_box_position ~box ~context ;
  clamp_width_or_height_to_content_size ~box ~measurement:`Width ~context ;
  clamp_width_or_height_to_content_size ~box ~measurement:`Height ~context ;
  handle_maximizing_of_inner_content_size ~parent_box:box
