let handle_vertical_scroll_on_evt ~y ~content ~scrollbar_box
    ~diff_from_initial_mousedown_to_start_of_bar =
  let Ui.{ top; bottom; _ } = Ui.get_box_sides ~box:content in
  Option.iter
    (fun bbox ->
      scrollbar_box.Ui.bbox <-
        Some
          {
            bbox with
            y =
              max top
                (min (bottom - bbox.Ui.height)
                   (y - diff_from_initial_mousedown_to_start_of_bar));
          })
    scrollbar_box.bbox

let handle_horizontal_scroll_on_evt ~x ~content ~scrollbar_box
    ~diff_from_initial_mousedown_to_start_of_bar =
  let Ui.{ left; right; _ } = Ui.get_box_sides ~box:content in
  Option.iter
    (fun bbox ->
      scrollbar_box.Ui.bbox <-
        Some
          {
            bbox with
            x =
              max left
                (min (right - bbox.Ui.width)
                   (x - diff_from_initial_mousedown_to_start_of_bar));
          })
    scrollbar_box.bbox

let get_scrollbar_event_logic ~content ~orientation =
  let original_mousedown_pos_was_within = ref false in
  let diff_from_initial_mousedown_to_start_of_bar = ref 0 in
  fun ~b ~e ->
    match e with
    | Sdl.MouseMotionEvt { x; y; _ } -> (
        match b with
        | Some b -> (
            match !Ui.holding_mousedown with
            | `True (~original_x, ~original_y) -> (
                let _, height_ratio =
                  Sdl.get_logical_to_opengl_window_dims_ratio ()
                in
                let y = y * height_ratio in
                if
                  Ui.is_within_box ~box:b ~x:original_x ~y:original_y
                    ~from_sdl_evt:true
                  && not !original_mousedown_pos_was_within
                then (
                  original_mousedown_pos_was_within := true;
                  let bbox = Option.value b.bbox ~default:Ui.default_bbox in
                  diff_from_initial_mousedown_to_start_of_bar :=
                    match orientation with
                    | Ui.Vertical -> y - bbox.y
                    | Ui.Horizontal -> x - bbox.x);
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

let create_scrollbar ~(content : Ui.box) ~(orientation : Ui.direction) =
  let content_bbox = Option.value content.bbox ~default:Ui.default_bbox in
  let evt_handler = get_scrollbar_event_logic ~content ~orientation in
  let scrollbar =
    {
      Ui.default_box with
      bbox =
        Some
          (match orientation with
          | Vertical -> { content_bbox with width = 8; height = 0 }
          | Horizontal -> { content_bbox with width = 0; height = 8 });
      background_color = (0., 0., 0., 1.);
      on_event = Some evt_handler;
    }
  in
  Ui_events.add_event_handler ~box:(Some scrollbar) ~event_handler:evt_handler;
  scrollbar

let create_scrollbar_container ~content ~orientation =
  let content_bbox = Option.value content.Ui.bbox ~default:Ui.default_bbox in
  let parent =
    match orientation with
    | Ui.Vertical ->
        Ui.
          {
            default_box with
            height_constraint = Some Max;
            bbox =
              Some
                {
                  content_bbox with
                  width = scrollbar_container_width;
                  height = 0;
                };
            background_color = (0.8, 0.8, 0.8, 1.);
            horizontal_align = Some Center;
            content = None;
          }
    | Ui.Horizontal ->
        Ui.
          {
            default_box with
            width_constraint = Some Max;
            bbox =
              Some
                {
                  content_bbox with
                  width = 0;
                  height = scrollbar_container_width;
                };
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
  let content_bbox = Option.value content.Ui.bbox ~default:Ui.default_bbox in
  let scrollcontainer =
    Ui.ScrollContainer
      {
        other_scrollcontainer;
        content;
        scroll = scrollbar;
        scrollbar_container;
        orientation;
        container =
          {
            Ui.default_box with
            bbox = Some { content_bbox with width = 0; height = 0 };
            width_constraint = Some Min;
            height_constraint = Some Min;
            content =
              Some
                (Boxes
                   [
                     (match other_scrollcontainer with
                     | Some scrollinfo ->
                         {
                           content with
                           content = Some (ScrollContainer scrollinfo);
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
  Ui.scrollcontainers := scrollcontainer :: !Ui.scrollcontainers;
  scrollcontainer

(* the code in this function reveals how fragile the design of the ui lib is.
I change the box contents and the focused box/element needs to be the direct box that holds
the contents so I have to change it here. Also text_wrap needs to be copied which is another edge case.
So many edge cases. *)
let wrap_box_contents_in_scrollcontainer ~(box : Ui.box) ~orientation =
  match box.content with
  | Some (ScrollContainer ({ content; _ } as scrollinfo)) -> (
      let scrollcontainer =
        create_scrollcontainer ~content ~orientation
          ~other_scrollcontainer:(Some scrollinfo)
      in
      match scrollcontainer with
      | ScrollContainer { container; _ } ->
          Ui.constrain_width_height ~box:container;
          Option.iter
            (fun bbox ->
              match orientation with
              | Ui.Vertical ->
                  container.bbox <-
                    Some
                      {
                        bbox with
                        width = bbox.Ui.width - Ui.scrollbar_container_width;
                      }
              | Horizontal ->
                  container.bbox <-
                    Some
                      {
                        bbox with
                        height = bbox.Ui.height - Ui.scrollbar_container_width;
                      })
            container.bbox;
          box.content <- Some scrollcontainer
      | _ -> ())
  | Some (Box _ | Boxes _ | Textarea _ | Text _ | TextAreaWithLineNumbers _)
    -> (
      let box_shallow_copy =
        {
          box with
          content = box.content;
          background_color = Ui.default_box.background_color;
        }
      in
      if
        Option.is_some !Ui.focused_element
        && Option.get !Ui.focused_element == box
      then Ui.set_focused_element ~box:box_shallow_copy;
      Option.iter
        (fun bbox ->
          match orientation with
          | Ui.Vertical ->
              box_shallow_copy.bbox <-
                Some
                  {
                    bbox with
                    width = bbox.Ui.width - Ui.scrollbar_container_width;
                  }
          | Horizontal ->
              box_shallow_copy.bbox <-
                Some
                  {
                    bbox with
                    height = bbox.Ui.height - Ui.scrollbar_container_width;
                  })
        box_shallow_copy.bbox;
      let scrollcontainer =
        create_scrollcontainer ~content:box_shallow_copy ~orientation
          ~other_scrollcontainer:None
      in
      match scrollcontainer with
      | ScrollContainer { container; _ } ->
          Ui.constrain_width_height ~box:container;
          box.on_event <- None;
          box.content <- Some scrollcontainer
      | _ -> ())
  | None -> failwith "cannot wrap box with no contents"

let unwrap_scrollcontainer ~(box : Ui.box) ~unwrap_orientation =
  match box.content with
  | Some
      (ScrollContainer
         ({ content; orientation; other_scrollcontainer; scroll; _ } as
          scrollcontainer_info)) -> (
      let scroll_bbox = Option.get scroll.bbox in
      let measurement =
        match orientation with
        | Vertical -> scroll_bbox.height
        | Horizontal -> scroll_bbox.width
      in
      (if unwrap_orientation = orientation && measurement = 0 then
         match other_scrollcontainer with
         | Some inner_scrollcontainer ->
             box.content <- Some (ScrollContainer inner_scrollcontainer)
         | None ->
             box.content <- Some (Box content);

             Option.iter
               (fun bbox ->
                 content.bbox <-
                   Some
                     (match orientation with
                     | Vertical ->
                         {
                           bbox with
                           width = bbox.Ui.width + Ui.scrollbar_container_width;
                         }
                     | Horizontal ->
                         {
                           bbox with
                           height =
                             bbox.Ui.height + Ui.scrollbar_container_width;
                         }))
               content.bbox);

      match other_scrollcontainer with
      | Some { orientation; _ } when unwrap_orientation = orientation ->
          let scroll_bbox = Option.get scroll.bbox in
          let measurement =
            match orientation with
            | Vertical -> scroll_bbox.height
            | Horizontal -> scroll_bbox.width
          in
          if measurement = 0 then (
            box.content <-
              Some
                (ScrollContainer
                   { scrollcontainer_info with other_scrollcontainer = None });
            Option.iter
              (fun bbox ->
                content.bbox <-
                  Some
                    (match orientation with
                    | Vertical ->
                        {
                          bbox with
                          width = bbox.Ui.width + Ui.scrollbar_container_width;
                        }
                    | Horizontal ->
                        {
                          bbox with
                          height = bbox.Ui.height + Ui.scrollbar_container_width;
                        }))
              content.bbox)
      | _ -> ())
  | _ -> ()

let adjust_scrollbar_according_to_content_size ~content ~scroll ~orientation =
  match content.Ui.bbox with
  | Some _ -> (
      let Ui.{ left; right; top; bottom } = Ui.get_box_sides ~box:content in
      let Ui.
            {
              bottom = content_bottom;
              right = content_right;
              left = content_left;
              top = content_top;
            } =
        Ui.calculate_content_boundaries ~box:content
      in
      match orientation with
      | Ui.Vertical -> (
          let content_height = content_bottom - content_top
          and parent_height = bottom - top in
          match content_height > parent_height with
          | true ->
              Option.iter
                (fun bbox ->
                  let new_scrollbar_height =
                    parent_height * parent_height / content_height
                  in
                  if bbox.Ui.y + new_scrollbar_height > bottom then
                    scroll.Ui.bbox <-
                      Some
                        {
                          bbox with
                          y = bottom - new_scrollbar_height;
                          height = new_scrollbar_height;
                        }
                  else
                    scroll.bbox <-
                      Some { bbox with height = new_scrollbar_height })
                scroll.Ui.bbox
          | false ->
              Option.iter
                (fun bbox ->
                  scroll.bbox <- Some { bbox with y = top; height = 0 };
                  content.scroll_y_offset <- 0)
                scroll.bbox)
      | Horizontal -> (
          let content_width = content_right - content_left
          and parent_width = right - left in
          match content_width > parent_width with
          | true ->
              Option.iter
                (fun bbox ->
                  let new_scrollbar_width =
                    parent_width * parent_width / content_width
                  in
                  if bbox.Ui.x + new_scrollbar_width > right then
                    scroll.Ui.bbox <-
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
                (fun bbox ->
                  scroll.bbox <- Some { bbox with x = left; width = 0 };
                  content.scroll_x_offset <- 0)
                scroll.bbox))
  | None -> ()

let change_content_scroll_offsets_based_off_scrollbar ~content ~scroll
    ~orientation =
  match scroll.Ui.bbox with
  | Some scroll_bbox ->
      let Ui.{ left; right; top; bottom } = Ui.get_box_sides ~box:content in
      let Ui.
            {
              left = content_left;
              right = content_right;
              top = content_top;
              bottom = content_bottom;
            } =
        Ui.calculate_content_boundaries ~box:content
      in
      let content_width = content_right - content_left in
      let content_height = content_bottom - content_top in
      let content_bbox_width = right - left in
      let content_bbox_height = bottom - top in
      (* distance from scrollbar to start of content divided by content size gives us the percentage
         that is supposed offscreen and I multiply by the content size to get how many pixels should be scrolled.
         negative because it needs to go in the opposite direction of the scrolling *)
      if scroll_bbox.width > 0 && orientation = Ui.Horizontal then
        content.scroll_x_offset <-
          -content_width * (scroll_bbox.x - left) / content_bbox_width;
      if scroll_bbox.height > 0 && orientation = Vertical then
        content.scroll_y_offset <-
          -content_height * (scroll_bbox.y - top) / content_bbox_height
  | _ -> ()
