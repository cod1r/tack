open Ui_types

let handle_vertical_scroll_on_evt
      ~y
      ~content
      ~scrollbar_box
      ~diff_from_initial_mousedown_to_start_of_bar
  =
  let { top; bottom; _ } = Ui.get_box_sides ~box:content in
  match scrollbar_box.bbox with
  | Some bbox ->
    scrollbar_box.bbox
    <- Some
         { bbox with
           y =
             max
               top
               (min
                  (bottom - bbox.height)
                  (y - diff_from_initial_mousedown_to_start_of_bar))
         }
  | None -> failwith "SHOULD HAVE BBOX FOR scrollbar_box.bbox"
;;

let handle_horizontal_scroll_on_evt
      ~x
      ~content
      ~scrollbar_box
      ~diff_from_initial_mousedown_to_start_of_bar
  =
  let { left; right; _ } = Ui.get_box_sides ~box:content in
  match scrollbar_box.bbox with
  | Some bbox ->
    scrollbar_box.bbox
    <- Some
         { bbox with
           x =
             max
               left
               (min
                  (right - bbox.width)
                  (x - diff_from_initial_mousedown_to_start_of_bar))
         }
  | None -> failwith "SHOULD HAVE BBOX FOR scrollbar_box.bbox"
;;

let get_scrollbar_event_logic ~content ~orientation =
  let original_mousedown_pos_was_within = ref false in
  let diff_from_initial_mousedown_to_start_of_bar = ref 0 in
  fun ~b ~e ->
    match e with
    | Sdl.MouseMotionEvt { x; y; _ } ->
      (match b with
       | Some b ->
         (match !Ui_globals.holding_mousedown with
          | `True (~original_x, ~original_y) ->
            let width_ratio, height_ratio =
              Sdl.get_logical_to_opengl_window_dims_ratio ()
            in
            let y = y * height_ratio in
            let x = x * width_ratio in
            if
              Ui.is_within_box ~box:b ~x:original_x ~y:original_y ~from_sdl_evt:true
              && not !original_mousedown_pos_was_within
            then (
              original_mousedown_pos_was_within := true;
              assert (b.bbox <> None);
              let bbox = Option.get b.bbox in
              diff_from_initial_mousedown_to_start_of_bar
              := match orientation with
                 | Vertical -> y - bbox.y
                 | Horizontal -> x - bbox.x);
            if !original_mousedown_pos_was_within
            then (
              match orientation with
              | Vertical ->
                handle_vertical_scroll_on_evt
                  ~y
                  ~content
                  ~scrollbar_box:b
                  ~diff_from_initial_mousedown_to_start_of_bar:
                    !diff_from_initial_mousedown_to_start_of_bar
              | Horizontal ->
                handle_horizontal_scroll_on_evt
                  ~x
                  ~content
                  ~scrollbar_box:b
                  ~diff_from_initial_mousedown_to_start_of_bar:
                    !diff_from_initial_mousedown_to_start_of_bar)
          | `False -> original_mousedown_pos_was_within := false)
       | None -> ())
    | _ -> ()
;;

let create_scrollbar ~(content : box) ~(orientation : direction) =
  let content_bbox = Option.value content.bbox ~default:Ui.default_bbox in
  let evt_handler = get_scrollbar_event_logic ~content ~orientation in
  let scrollbar =
    { Ui.default_box with
      bbox =
        Some
          (match orientation with
           | Vertical -> { content_bbox with width = 8; height = 0 }
           | Horizontal -> { content_bbox with width = 0; height = 8 })
    ; background_color = 0., 0., 0., 1.
    ; on_event = Some evt_handler
    }
  in
  Ui_events.add_event_handler ~box:(Some scrollbar) ~event_handler:evt_handler;
  scrollbar
;;

let create_scrollbar_container ~(content : box) ~orientation =
  assert (content.bbox <> None);
  let content_bbox = Option.get content.bbox in
  let parent =
    match orientation with
    | Vertical ->
      { Ui.default_box with
        name = Some "V_SCROLLBAR_CONTAINER"
      ; height_constraint = Some Max
      ; bbox =
          Some
            { content_bbox with width = Ui_globals.scrollbar_container_width; height = 0 }
      ; background_color = 0.8, 0.8, 0.8, 1.
      ; horizontal_align = Some Center
      ; content = None
      }
    | Horizontal ->
      { Ui.default_box with
        name = Some "H_SCROLLBAR_CONTAINER"
      ; width_constraint = Some Max
      ; bbox =
          Some
            { content_bbox with width = 0; height = Ui_globals.scrollbar_container_width }
      ; background_color = 0.8, 0.8, 0.8, 1.
      ; vertical_align = Some Center
      ; content = None
      }
  in
  let scrollbar = create_scrollbar ~content ~orientation in
  parent.content <- Some (Box scrollbar);
  ~scrollbar_container:parent, ~scrollbar
;;

let create_vertical_scrollcontainer ~(content : box) =
  let ~scrollbar_container, ~scrollbar =
    create_scrollbar_container ~content ~orientation:Vertical
  in
  { vertical_scroll = scrollbar; vertical_scrollbar_container = scrollbar_container }
;;

let create_horizontal_scrollcontainer ~(content : box) =
  let ~scrollbar_container, ~scrollbar =
    create_scrollbar_container ~content ~orientation:Horizontal
  in
  { horizontal_scroll = scrollbar; horizontal_scrollbar_container = scrollbar_container }
;;

let adjust_scrollbar_container_according_to_content_size ~scrollcontainer_info ~content =
  let { vertical_scroll_info; horizontal_scroll_info } = scrollcontainer_info in
  assert (content.bbox <> None);
  let { left; right; top; bottom } = Ui.get_box_sides ~box:content in
  let { bottom = content_bottom
      ; right = content_right
      ; left = content_left
      ; top = content_top
      }
    =
    Ui.calculate_content_boundaries ~box:content
  in
  (match vertical_scroll_info with
   | Some { vertical_scroll = scroll; vertical_scrollbar_container = scrollbar_container }
     ->
     assert (scrollbar_container.bbox <> None);
     let scrollbar_container_bbox = Option.get scrollbar_container.bbox in
     scrollbar_container.bbox
     <- Some { scrollbar_container_bbox with x = right; height = bottom - top };
     assert (scroll.bbox <> None);
     let bbox = Option.get scroll.bbox in
     let content_height = content_bottom - content_top
     and parent_height = bottom - top in
     (match content_height > parent_height with
      | true ->
        let new_scrollbar_height = parent_height * parent_height / content_height in
        scroll.bbox
        <- Some
             { bbox with
               height = new_scrollbar_height
             ; y =
                 (if bbox.y + new_scrollbar_height > bottom
                  then bottom - new_scrollbar_height
                  else if bbox.y < top
                  then scrollbar_container_bbox.y
                  else bbox.y)
             }
      | false ->
        scroll.bbox <- Some { bbox with y = top; height = 0 };
        content.scroll_y_offset <- 0)
   | None -> ());
  match horizontal_scroll_info with
  | Some
      { horizontal_scroll = scroll; horizontal_scrollbar_container = scrollbar_container }
    ->
    assert (scrollbar_container.bbox <> None);
    let scrollbar_container_bbox = Option.get scrollbar_container.bbox in
    scrollbar_container.bbox
    <- Some { scrollbar_container_bbox with y = bottom; width = right - left };
    assert (scroll.bbox <> None);
    let bbox = Option.get scroll.bbox in
    let content_width = content_right - content_left
    and parent_width = right - left in
    (match content_width > parent_width with
     | true ->
       let new_scrollbar_width = parent_width * parent_width / content_width in
       scroll.bbox
       <- Some
            { bbox with
              width = new_scrollbar_width
            ; x =
                (if bbox.x + new_scrollbar_width > right
                 then right - new_scrollbar_width
                 else if bbox.x < left
                 then scrollbar_container_bbox.x
                 else bbox.x)
            }
     | false ->
       scroll.bbox <- Some { bbox with x = left; width = 0 };
       content.scroll_x_offset <- 0)
  | None -> ()
;;

let change_content_scroll_offsets_based_off_scrollbar ~scrollcontainer_info ~content =
  let { vertical_scroll_info; horizontal_scroll_info } = scrollcontainer_info in
  let { left; right; top; bottom } = Ui.get_box_sides ~box:content in
  let { left = content_left
      ; right = content_right
      ; top = content_top
      ; bottom = content_bottom
      }
    =
    Ui.calculate_content_boundaries ~box:content
  in
  let content_width = content_right - content_left in
  let content_height = content_bottom - content_top in
  let content_bbox_width = right - left in
  let content_bbox_height = bottom - top in
  (match vertical_scroll_info with
   | Some { vertical_scroll = scroll; _ } ->
     (match scroll.bbox with
      | Some scroll_bbox ->
        (* distance from scrollbar to start of content divided by content size gives us the percentage
         that is supposed offscreen and I multiply by the content size to get how many pixels should be scrolled.
         negative because it needs to go in the opposite direction of the scrolling *)
        if scroll_bbox.height > 0
        then
          content.scroll_y_offset
          <- -content_height * (scroll_bbox.y - top) / content_bbox_height
      | None -> ())
   | None -> ());
  match horizontal_scroll_info with
  | Some { horizontal_scroll = scroll; _ } ->
    (match scroll.bbox with
     | Some scroll_bbox ->
       if scroll_bbox.width > 0
       then
         content.scroll_x_offset
         <- -content_width * (scroll_bbox.x - left) / content_bbox_width
     | None -> ())
  | None -> ()
;;
