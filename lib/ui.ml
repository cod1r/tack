type bounding_box = {
  mutable width : int;
  mutable height : int;
  mutable x : int;
  mutable y : int;
}

type direction = Horizontal | Vertical
type box_sides = { left : int; right : int; top : int; bottom : int }
type positioning = Relative of { x : int; y : int } | Absolute
type horizontal_alignment = Left | Center | Right
type vertical_alignment = Top | Center | Bottom
type size_constraint = Min | Max

type box = {
  mutable name : string option;
  mutable content : box_content option;
  mutable bbox : bounding_box option;
  mutable text_wrap : bool;
  mutable background_color : float * float * float * float;
  mutable border : bool;
  mutable flow : direction option;
  mutable font_size : int option;
  mutable width_constraint : size_constraint option;
  mutable height_constraint : size_constraint option;
  mutable clip_content : bool;
  mutable position_type : positioning;
  mutable allow_horizontal_scroll : bool;
  mutable allow_vertical_scroll : bool;
  mutable horizontal_align : horizontal_alignment option;
  mutable vertical_align : vertical_alignment option;
  on_event : event_handler_t option;
  mutable scroll_x_offset : int;
  mutable scroll_y_offset : int;
}

and event_handler_t = b:box option -> e:Sdl.event -> unit

and text_area_information = {
  text : Rope.rope option;
  cursor_pos : int option;
  highlight_pos : int option * int option;
  holding_mousedown_rope_pos : int option;
}

and scrollcontainer_info = {
  other_scrollcontainer : scrollcontainer_info option;
  content : box;
  scroll : box;
  scrollbar_container : box;
  container : box;
  orientation : direction;
}

and box_content =
  | Box of box
  | Boxes of box list
  | Text of string
  | Textarea of text_area_information
  | ScrollContainer of scrollcontainer_info

let focused_element : box option ref = ref None
let set_focused_element ~(box : box) = focused_element := Some box
let unfocus_element () = focused_element := None
let default_bbox : bounding_box = { width = 0; height = 0; x = 0; y = 0 }

let holding_mousedown :
    [ `True of original_x:int * original_y:int | `False ] ref =
  ref `False

let holding_ctrl = ref false

let get_box_sides ~(box : box) : box_sides =
  match box.bbox with
  | Some bbox ->
      let right = bbox.x + bbox.width and bottom = bbox.y + bbox.height in
      { left = bbox.x; top = bbox.y; right; bottom }
  | None -> failwith "calling get_box_sides requires a bbox property of Some"

let default_text_area_information =
  {
    text = None;
    cursor_pos = None;
    highlight_pos = (None, None);
    holding_mousedown_rope_pos = None;
  }

let is_within_box ~x ~y ~box ~from_sdl_evt =
  let x, y =
    if from_sdl_evt then
      let width_ratio, height_ratio =
        Sdl.get_logical_to_opengl_window_dims_ratio ()
      in
      (x * width_ratio, y * height_ratio)
    else (x, y)
  in
  let { left; right; top; bottom } = get_box_sides ~box in
  x >= left && x <= right && y <= bottom && y >= top

let default_textarea_event_handler =
 fun ~b ~e ->
  match b with
  | Some b -> (
      match b.content with
      | Some (Textarea info) -> (
          match e with
          | Sdl.MouseButtonEvt { x; y; _ } -> (
              match b.bbox with
              | Some _ ->
                  if is_within_box ~x ~y ~from_sdl_evt:true ~box:b then
                    set_focused_element ~box:b
              | None -> ())
          | _ -> ())
      | _ -> ())
  | _ -> ()

let default_box =
  {
    name = None;
    content = None;
    bbox = None;
    text_wrap = true;
    background_color = (1., 1., 1., 1.);
    border = false;
    font_size = None;
    width_constraint = None;
    height_constraint = None;
    clip_content = false;
    position_type = Relative { x = 0; y = 0 };
    allow_horizontal_scroll = false;
    allow_vertical_scroll = false;
    horizontal_align = None;
    vertical_align = None;
    flow = None;
    on_event = None;
    scroll_x_offset = 0;
    scroll_y_offset = 0;
  }

let get_glyph_info_from_glyph ~glyph ~font_info =
  let opt =
    Array.find_opt
      (fun (c', _) -> c' = glyph)
      font_info.Freetype.glyph_info_with_char
  in
  try
    let _, gi = Option.get opt in
    gi
  with Invalid_argument e -> failwith (__FUNCTION__ ^ "; " ^ e)

let get_text_wrap_info ~bbox ~glyph ~x ~y ~font_info ~text_wrap =
  if glyph = '\n' then
    (~new_x:bbox.x, ~new_y:(y + font_info.Freetype.font_height), ~wraps:true)
  else
    let glyph_info = get_glyph_info_from_glyph ~glyph ~font_info in
    if x + glyph_info.x_advance > bbox.x + bbox.width && text_wrap then
      ( ~new_x:(bbox.x + glyph_info.x_advance),
        ~new_y:(y + font_info.font_height),
        ~wraps:true )
    else (~new_x:(x + glyph_info.x_advance), ~new_y:y, ~wraps:false)

module TextTextureInfo = struct
  type texture_info = {
    gl_texture_id : int;
    font_size : int;
    font_info : Freetype.font_info;
  }

  let text_textures_with_different_font_sizes : texture_info list ref = ref []

  let get_or_add_font_size_text_texture ~(font_size : int) =
    let option =
      List.find_opt
        (fun { font_size = font_size'; _ } -> font_size' = font_size)
        !text_textures_with_different_font_sizes
    in
    match option with
    | Some { font_info; gl_texture_id; _ } -> (~font_info, ~gl_texture_id)
    | None ->
        let gl_buffer_glyph_texture_atlas = Opengl.gl_gen_texture () in
        let font_info =
          Freetype.get_new_font_info_with_font_size ~font_size
            ~face:Freetype.face
        in
        text_textures_with_different_font_sizes :=
          {
            gl_texture_id = gl_buffer_glyph_texture_atlas;
            font_size;
            font_info;
          }
          :: !text_textures_with_different_font_sizes;
        Opengl.gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
        Opengl.set_gl_tex_parameters_ui_text ();
        Opengl.gl_teximage_2d ~bytes:font_info.font_texture_atlas.bytes
          ~width:font_info.font_texture_atlas.width
          ~height:font_info.font_texture_atlas.height;
        (~font_info, ~gl_texture_id:gl_buffer_glyph_texture_atlas)
end

let create_textarea_box () =
  {
    default_box with
    content = Some (Textarea default_text_area_information);
    on_event = Some default_textarea_event_handler;
  }

let text_caret_width = 3

let get_text_bounding_box ~(box : box) =
  let rope, is_textarea =
    match box.content with
    | Some (Text s) -> (Rope.of_string s, false)
    | Some (Textarea { text; _ }) ->
        ((match text with Some r -> r | None -> Rope.of_string ""), true)
    | _ -> failwith __FUNCTION__
  in
  let bbox = Option.value box.bbox ~default:default_bbox in
  let min_x, min_y, max_x, max_y =
    (ref bbox.x, ref bbox.y, ref Int.min_int, ref Int.min_int)
  in
  let ~font_info, .. =
    TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
  in
  let Rope.{ x; y; _ } =
    Rope.traverse_rope ~rope
      ~handle_result:(fun
          (acc : Rope.rope_traversal_info Rope.traverse_info) c ->
        let (Rope_Traversal_Info acc) = acc in
        max_y := max !max_y acc.y;
        match c with
        | '\n' ->
            Rope_Traversal_Info
              {
                y = acc.y + font_info.Freetype.font_height;
                x = bbox.x;
                rope_pos = acc.rope_pos + 1;
              }
        | _ ->
            let ~new_x, ~new_y, .. =
              get_text_wrap_info ~bbox ~glyph:c ~x:acc.x ~y:acc.y ~font_info
                ~text_wrap:box.text_wrap
            in
            max_x :=
              max !max_x (new_x + if is_textarea then text_caret_width else 0);
            Rope_Traversal_Info
              { y = new_y; x = new_x; rope_pos = acc.rope_pos + 1 })
      ~result:(Rope_Traversal_Info { x = bbox.x; y = bbox.y; rope_pos = 0 })
  in
  max_y := max !max_y y;
  max_y := !max_y + font_info.font_height;
  (~min_x:!min_x, ~max_x:!max_x, ~min_y:!min_y, ~max_y:!max_y)

(* I thought this would be simpler to calculate but sometimes the box's bbox doesn't reflect the potential size that
its contents could take up, so recursion is necessary *)
let rec calculate_content_boundaries ~(box : box) =
  let { left; right; top; bottom } = get_box_sides ~box in
  match box.content with
  | Some (Box b) ->
      let { left = left'; right = right'; top = top'; bottom = bottom' } =
        calculate_content_boundaries ~box:b
      in
      {
        left = min left left';
        right = max right right';
        top = min top top';
        bottom = max bottom bottom';
      }
  | Some (Boxes list) ->
      let ( min_horizontal_position,
            max_horizontal_position,
            min_vertical_position,
            max_vertical_position ) =
        List.fold_left
          (fun (min_horizontal, max_horizontal, min_vertical, max_vertical) b ->
            let { left; right; top; bottom } = get_box_sides ~box:b in
            ( min min_horizontal left,
              max max_horizontal right,
              min min_vertical top,
              max max_vertical bottom ))
          (left, right, top, bottom) list
      in
      {
        left = min_horizontal_position;
        right = max_horizontal_position;
        top = min_vertical_position;
        bottom = max_vertical_position;
      }
  | Some (Text _) | Some (Textarea _) ->
      let ~min_x, ~max_x, ~min_y, ~max_y = get_text_bounding_box ~box in
      {
        left = min left min_x;
        right = max right max_x;
        top = min top min_y;
        bottom = max bottom max_y;
      }
  | Some (ScrollContainer { container; _ }) ->
      calculate_content_boundaries ~box:container
  | None -> { left; right; top; bottom }

let handle_vertical_scroll_on_evt ~y ~content ~scrollbar_box
    ~diff_from_initial_mousedown_to_start_of_bar =
  let { top; bottom; _ } = get_box_sides ~box:content in
  Option.iter
    (fun bbox ->
      scrollbar_box.bbox <-
        Some
          {
            bbox with
            y =
              max top
                (min (bottom - bbox.height)
                   (y - diff_from_initial_mousedown_to_start_of_bar));
          })
    scrollbar_box.bbox

let handle_horizontal_scroll_on_evt ~x ~content ~scrollbar_box
    ~diff_from_initial_mousedown_to_start_of_bar =
  let { left; right; _ } = get_box_sides ~box:content in
  Option.iter
    (fun bbox ->
      scrollbar_box.bbox <-
        Some
          {
            bbox with
            x =
              max left
                (min (right - bbox.width)
                   (x - diff_from_initial_mousedown_to_start_of_bar));
          })
    scrollbar_box.bbox

let change_content_scroll_offsets_based_off_scrollbar ~content ~scroll
    ~orientation =
  match scroll.bbox with
  | Some scroll_bbox ->
      let { left; right; top; bottom } = get_box_sides ~box:content in
      let {
        left = content_left;
        right = content_right;
        top = content_top;
        bottom = content_bottom;
      } =
        calculate_content_boundaries ~box:content
      in
      let content_width = content_right - left in
      let content_height = content_bottom - top in
      let content_bbox_width = right - left in
      let content_bbox_height = bottom - top in
      (* distance from scrollbar to start of content divided by content size gives us the percentage
         that is supposed offscreen and I multiply by the content size to get how many pixels should be scrolled.
         negative because it needs to go in the opposite direction of the scrolling *)
      if scroll_bbox.width > 0 && orientation = Horizontal then
        content.scroll_x_offset <-
          -content_width * (scroll_bbox.x - left) / content_bbox_width;
      if scroll_bbox.height > 0 && orientation = Vertical then
        content.scroll_y_offset <-
          -content_height * (scroll_bbox.y - top) / content_bbox_height
  | _ -> ()

let get_xy_pos_of_text_caret ~text_area_info ~box =
  let bbox = Option.value box.bbox ~default:default_bbox in
  if Option.is_none text_area_info.cursor_pos then None
  else
    let ~font_info, .. =
      TextTextureInfo.get_or_add_font_size_text_texture
        ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
    in
    match text_area_info.text with
    | Some rope ->
        let Rope.{ x; y; _ } =
          let start_x = bbox.x + box.scroll_x_offset
          and start_y = bbox.y + box.scroll_y_offset in
          Rope.traverse_rope ~rope
            ~handle_result:(fun acc c ->
              let (Rope_Traversal_Info acc) = acc in
              if acc.rope_pos = Option.get text_area_info.cursor_pos then
                Rope_Traversal_Info acc
              else
                match c with
                | '\n' ->
                    Rope_Traversal_Info
                      {
                        x = start_x;
                        y = acc.y + font_info.font_height;
                        rope_pos = acc.rope_pos + 1;
                      }
                | _ ->
                    let ~new_x, ~new_y, ~wraps =
                      get_text_wrap_info
                        ~bbox:(Option.value box.bbox ~default:default_bbox)
                        ~glyph:c ~x:acc.x ~y:acc.y ~font_info
                        ~text_wrap:box.text_wrap
                    in
                    Rope_Traversal_Info
                      {
                        x = (if wraps then start_x else new_x);
                        y = new_y;
                        rope_pos = acc.rope_pos + 1;
                      })
            ~result:
              (Rope_Traversal_Info { x = start_x; y = start_y; rope_pos = 0 })
        in
        Some (~x, ~y)
    | None -> None

let textareas_and_their_scrollcontainers = ref []

let get_scrollcontainers_for_textarea ~(box_containing_textarea : box) =
  List.filter_map
    (fun (~scrollcontainer, ~content) ->
      if content == box_containing_textarea then Some scrollcontainer else None)
    !textareas_and_their_scrollcontainers

let adjust_scrollbar_according_to_textarea_text_caret' ~text_area_info ~scroll
    ~orientation ~content =
  let { right; left; top; bottom } = get_box_sides ~box:content in
  let {
    right = content_right;
    left = content_left;
    top = content_top;
    bottom = content_bottom;
  } =
    calculate_content_boundaries ~box:content
  in
  let ~font_info, .. =
    TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value content.font_size ~default:Freetype.font_size)
  in
  match get_xy_pos_of_text_caret ~text_area_info ~box:content with
  | Some (~x, ~y) ->
      Option.iter
        (fun bbox ->
          match orientation with
          | Horizontal ->
              if x + text_caret_width > right then
                scroll.bbox <-
                  Some
                    {
                      bbox with
                      x =
                        bbox.x
                        + (right - left)
                          * (x + text_caret_width - right)
                          / (content_right - content_left);
                    };
              if x < left then
                scroll.bbox <-
                  Some
                    {
                      bbox with
                      x =
                        bbox.x
                        + (right - left) * (x - left)
                          / (content_right - content_left);
                    }
          | _ -> ())
        scroll.bbox;
      Option.iter
        (fun bbox ->
          match orientation with
          | Vertical ->
              if y < top then
                scroll.bbox <-
                  Some
                    {
                      bbox with
                      y =
                        bbox.y
                        + (bottom - top) * (y - top)
                          / (content_bottom - content_top);
                    };
              if y + font_info.font_height > bottom then
                scroll.bbox <-
                  Some
                    {
                      bbox with
                      y =
                        bbox.y
                        + (bottom - top)
                          * (y + font_info.font_height - bottom)
                          / (content_bottom - content_top);
                    }
          | _ -> ())
        scroll.bbox
  | None -> ()

let adjust_scrollbar_according_to_textarea_text_caret
    ~(box_containing_textarea : box) ~text_area_info =
  let list = get_scrollcontainers_for_textarea ~box_containing_textarea in
  List.iter
    (fun scrollcontainer ->
      match scrollcontainer with
      | ScrollContainer { orientation; scroll; content; _ } ->
          adjust_scrollbar_according_to_textarea_text_caret' ~orientation
            ~scroll ~content ~text_area_info
      | _ -> ())
    list

let adjust_scrollbar_according_to_content_size ~content ~scroll ~orientation =
  match content.bbox with
  | Some _ -> (
      let { left; right; top; bottom } = get_box_sides ~box:content in
      let { bottom = content_bottom; right = content_right; _ } =
        calculate_content_boundaries ~box:content
      in
      match orientation with
      | Vertical -> (
          let content_height = content_bottom - top
          and parent_height = bottom - top in
          match content_height > parent_height with
          | true ->
              Option.iter
                (fun bbox ->
                  let new_scrollbar_height =
                    parent_height * parent_height / content_height
                  in
                  if bbox.y + new_scrollbar_height > bottom then
                    scroll.bbox <-
                      Some
                        {
                          bbox with
                          y = bottom - new_scrollbar_height;
                          height = new_scrollbar_height;
                        }
                  else
                    scroll.bbox <-
                      Some { bbox with height = new_scrollbar_height })
                scroll.bbox
          | false ->
              Option.iter
                (fun bbox -> scroll.bbox <- Some { bbox with height = 0 })
                scroll.bbox)
      | Horizontal -> (
          let content_width = content_right - left
          and parent_width = right - left in
          match content_width > parent_width with
          | true ->
              Option.iter
                (fun bbox ->
                  let new_scrollbar_width =
                    parent_width * parent_width / content_width
                  in
                  if bbox.x + new_scrollbar_width > right then
                    scroll.bbox <-
                      Some
                        {
                          bbox with
                          x = right - new_scrollbar_width;
                          width = new_scrollbar_width;
                        }
                  else
                    scroll.bbox <-
                      Some { bbox with width = new_scrollbar_width })
                scroll.bbox
          | false ->
              Option.iter
                (fun bbox -> scroll.bbox <- Some { bbox with width = 0 })
                scroll.bbox))
  | None -> ()

let get_scrollbar_event_logic ~content ~orientation =
  let original_mousedown_pos_was_within = ref false in
  let diff_from_initial_mousedown_to_start_of_bar = ref 0 in
  fun ~b ~e ->
    match e with
    | Sdl.MouseMotionEvt { x; y; _ } -> (
        match b with
        | Some b -> (
            match !holding_mousedown with
            | `True (~original_x, ~original_y) -> (
                let _, height_ratio =
                  Sdl.get_logical_to_opengl_window_dims_ratio ()
                in
                let y = y * height_ratio in
                if
                  is_within_box ~box:b ~x:original_x ~y:original_y
                    ~from_sdl_evt:true
                  && not !original_mousedown_pos_was_within
                then (
                  original_mousedown_pos_was_within := true;
                  let bbox = Option.value b.bbox ~default:default_bbox in
                  diff_from_initial_mousedown_to_start_of_bar :=
                    match orientation with
                    | Vertical -> y - bbox.y
                    | Horizontal -> x - bbox.x);
                if !original_mousedown_pos_was_within then
                  match orientation with
                  | Vertical ->
                      handle_vertical_scroll_on_evt ~y ~content ~scrollbar_box:b
                        ~diff_from_initial_mousedown_to_start_of_bar:
                          !diff_from_initial_mousedown_to_start_of_bar
                  | Horizontal ->
                      handle_horizontal_scroll_on_evt ~x ~content
                        ~scrollbar_box:b
                        ~diff_from_initial_mousedown_to_start_of_bar:
                          !diff_from_initial_mousedown_to_start_of_bar)
            | `False -> original_mousedown_pos_was_within := false)
        | None -> ())
    | _ -> ()

let create_scrollbar ~content ~orientation =
  let content_bbox = Option.value content.bbox ~default:default_bbox in
  {
    default_box with
    bbox =
      Some
        (match orientation with
        | Vertical -> { content_bbox with width = 8; height = 0 }
        | Horizontal -> { content_bbox with width = 0; height = 8 });
    background_color = (0., 0., 0., 1.);
    on_event = Some (get_scrollbar_event_logic ~content ~orientation);
  }

let create_scrollbar_container ~content ~orientation =
  let content_bbox = Option.value content.bbox ~default:default_bbox in
  let parent =
    match orientation with
    | Vertical ->
        {
          default_box with
          height_constraint = Some Max;
          bbox = Some { content_bbox with width = 15; height = 0 };
          background_color = (0.8, 0.8, 0.8, 1.);
          horizontal_align = Some Center;
          content = None;
        }
    | Horizontal ->
        {
          default_box with
          width_constraint = Some Max;
          bbox = Some { content_bbox with width = 0; height = 15 };
          background_color = (0.8, 0.8, 0.8, 1.);
          vertical_align = Some Center;
          content = None;
        }
  in
  let scrollbar = create_scrollbar ~content ~orientation in
  parent.content <- Some (Box scrollbar);
  (~scrollbar_container:parent, ~scrollbar)

let create_scrollcontainer ~content ~orientation ~other_scrollcontainer =
  let ~scrollbar_container, ~scrollbar =
    create_scrollbar_container ~content ~orientation
  in
  let content_bbox = Option.value content.bbox ~default:default_bbox in
  let scrollcontainer =
    ScrollContainer
      {
        other_scrollcontainer;
        content;
        scroll = scrollbar;
        scrollbar_container;
        orientation;
        container =
          {
            default_box with
            bbox = Some { content_bbox with width = 0; height = 0 };
            width_constraint = Some Min;
            height_constraint = Some Min;
            content =
              Some
                (Boxes
                   [
                     (match other_scrollcontainer with
                     | Some info ->
                         {
                           default_box with
                           bbox =
                             Some { content_bbox with width = 0; height = 0 };
                           height_constraint = Some Min;
                           width_constraint = Some Min;
                           content = Some (ScrollContainer info);
                         }
                     | None -> content);
                     scrollbar_container;
                   ]);
            flow =
              Some
                (match orientation with
                | Vertical -> Horizontal
                | Horizontal -> Vertical);
          };
      }
  in
  textareas_and_their_scrollcontainers :=
    (~scrollcontainer, ~content) :: !textareas_and_their_scrollcontainers;
  scrollcontainer

let clone_box ~(box : box) =
  let visited = ref [] in
  let rec clone_box' box =
    if List.exists (fun b -> b == box) !visited then
      failwith "Recursive structure detected"
    else visited := box :: !visited;
    {
      name = box.name;
      content =
        (match box.content with
        | Some (Box b) -> Some (Box (clone_box' b))
        | Some (Boxes list) ->
            Some (Boxes (List.map (fun b -> clone_box' b) list))
        | Some
            (ScrollContainer
               {
                 container;
                 content;
                 scroll;
                 orientation;
                 other_scrollcontainer;
                 _;
               }) ->
            let content = clone_box' content in
            Some
              (create_scrollcontainer ~content ~orientation
                 ~other_scrollcontainer)
        | Some (Text _) | Some (Textarea _) | None -> box.content);
      bbox = box.bbox;
      text_wrap = box.text_wrap;
      background_color = box.background_color;
      border = box.border;
      flow = box.flow;
      font_size = box.font_size;
      width_constraint = box.width_constraint;
      height_constraint = box.height_constraint;
      clip_content = box.clip_content;
      position_type = box.position_type;
      allow_vertical_scroll = box.allow_vertical_scroll;
      allow_horizontal_scroll = box.allow_horizontal_scroll;
      horizontal_align = box.horizontal_align;
      vertical_align = box.vertical_align;
      on_event = box.on_event;
      scroll_y_offset = box.scroll_y_offset;
      scroll_x_offset = box.scroll_x_offset;
    }
  in
  clone_box' box
