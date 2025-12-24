open Ui_types

let handle_vertical_scroll_on_evt ~y ~content ~scrollbar_box
    ~diff_from_initial_mousedown_to_start_of_bar =
  let {top; bottom; _} = Ui.get_box_sides ~box:content in
  match scrollbar_box.bbox with
  | Some bbox ->
      scrollbar_box.bbox <-
        Some
          { bbox with
            y=
              max top
                (min (bottom - bbox.height)
                   (y - diff_from_initial_mousedown_to_start_of_bar) ) }
  | None ->
      failwith "SHOULD HAVE BBOX FOR scrollbar_box.bbox"

let handle_horizontal_scroll_on_evt ~x ~content ~scrollbar_box
    ~diff_from_initial_mousedown_to_start_of_bar =
  let {left; right; _} = Ui.get_box_sides ~box:content in
  match scrollbar_box.bbox with
  | Some bbox ->
      scrollbar_box.bbox <-
        Some
          { bbox with
            x=
              max left
                (min (right - bbox.width)
                   (x - diff_from_initial_mousedown_to_start_of_bar) ) }
  | None ->
      failwith "SHOULD HAVE BBOX FOR scrollbar_box.bbox"

let get_scrollbar_event_logic ~content ~orientation =
  let original_mousedown_pos_was_within = ref false in
  let diff_from_initial_mousedown_to_start_of_bar = ref 0 in
  fun ~b ~e ->
    match e with
    | Sdl.MouseMotionEvt {x; y; _} -> (
      match b with
      | Some b -> (
        match !Ui.holding_mousedown with
        | `True (~original_x, ~original_y) -> begin
          let width_ratio, height_ratio =
            Sdl.get_logical_to_opengl_window_dims_ratio ()
          in
          let y = y * height_ratio in
          let x = x * width_ratio in
          if
            Ui.is_within_box ~box:b ~x:original_x ~y:original_y
              ~from_sdl_evt:true
            && not !original_mousedown_pos_was_within
          then begin
            original_mousedown_pos_was_within := true ;
            assert (b.bbox <> None) ;
            let bbox = Option.get b.bbox in
            diff_from_initial_mousedown_to_start_of_bar :=
              match orientation with
              | Vertical ->
                  y - bbox.y
              | Horizontal ->
                  x - bbox.x
          end ;
          if !original_mousedown_pos_was_within then begin
            match orientation with
            | Vertical ->
                handle_vertical_scroll_on_evt ~y ~content ~scrollbar_box:b
                  ~diff_from_initial_mousedown_to_start_of_bar:
                    !diff_from_initial_mousedown_to_start_of_bar
            | Horizontal ->
                handle_horizontal_scroll_on_evt ~x ~content ~scrollbar_box:b
                  ~diff_from_initial_mousedown_to_start_of_bar:
                    !diff_from_initial_mousedown_to_start_of_bar
          end
          end
        | `False ->
            original_mousedown_pos_was_within := false )
      | None ->
          () )
    | _ ->
        ()

let create_scrollbar ~(content : box) ~(orientation : direction) =
  let content_bbox = Option.value content.bbox ~default:Ui.default_bbox in
  let evt_handler = get_scrollbar_event_logic ~content ~orientation in
  let scrollbar =
    { Ui.default_box with
      bbox=
        Some
          ( match orientation with
          | Vertical ->
              {content_bbox with width= 8; height= 0}
          | Horizontal ->
              {content_bbox with width= 0; height= 8} )
    ; background_color= (0., 0., 0., 1.)
    ; on_event= Some evt_handler }
  in
  Ui_events.add_event_handler ~box:(Some scrollbar) ~event_handler:evt_handler ;
  scrollbar

let create_scrollbar_container ~(content : box) ~orientation =
  let content_bbox = Option.value content.bbox ~default:Ui.default_bbox in
  let parent =
    match orientation with
    | Vertical ->
        { Ui.default_box with
          name= Some "V_SCROLLBAR_CONTAINER"
        ; height_constraint= Some Max
        ; bbox=
            Some
              {content_bbox with width= Ui.scrollbar_container_width; height= 0}
        ; background_color= (0.8, 0.8, 0.8, 1.)
        ; horizontal_align= Some Center
        ; content= None }
    | Horizontal ->
        { Ui.default_box with
          name= Some "H_SCROLLBAR_CONTAINER"
        ; width_constraint= Some Max
        ; bbox=
            Some
              {content_bbox with width= 0; height= Ui.scrollbar_container_width}
        ; background_color= (0.8, 0.8, 0.8, 1.)
        ; vertical_align= Some Center
        ; content= None }
  in
  let scrollbar = create_scrollbar ~content ~orientation in
  parent.content <- Some (Box scrollbar) ;
  (~scrollbar_container:parent, ~scrollbar)

let create_scrollcontainer ~(content : box) ~orientation ~other_scrollcontainer
    =
  let ~scrollbar_container, ~scrollbar =
    create_scrollbar_container ~content ~orientation
  in
  assert (content.bbox <> None) ;
  let content_bbox = Option.get content.bbox in
  let scrollcontainer =
    ScrollContainer
      { other_scrollcontainer
      ; content
      ; scroll= scrollbar
      ; scrollbar_container
      ; orientation
      ; container=
          { Ui.default_box with
            bbox= Some {content_bbox with width= 0; height= 0}
          ; width_constraint= Some Min
          ; height_constraint= Some Min
          ; content=
              Some
                (Boxes
                   [ ( match other_scrollcontainer with
                     | Some scrollinfo ->
                         { content with
                           content= Some (ScrollContainer scrollinfo) }
                     | None ->
                         content )
                   ; scrollbar_container ] )
          ; flow=
              Some
                ( match orientation with
                | Vertical ->
                    Horizontal
                | Horizontal ->
                    Vertical ) } }
  in
  Ui.scrollcontainers := scrollcontainer :: !Ui.scrollcontainers ;
  scrollcontainer

let wrap_box_contents_in_scrollcontainer ~(parent : box) ~(box : box)
    ~orientation =
  match box.content with
  | Some (Box _ | Boxes _ | Textarea _ | Text _ | TextAreaWithLineNumbers _) ->
  begin
    begin match box.bbox with
    | Some bbox -> begin
      match orientation with
      | Vertical ->
          box.bbox <-
            Some {bbox with width= bbox.width - Ui.scrollbar_container_width}
      | Horizontal ->
          box.bbox <-
            Some {bbox with height= bbox.height - Ui.scrollbar_container_width}
      end
    | None ->
        failwith "SHOULD HAVE BBOX FOR box.bbox"
    end ;
    match parent.content with
    | Some (Boxes list) ->
        let other_scrollcontainer =
          List.find_map
            (fun (b : box) ->
              match b.content with
              | Some (ScrollContainer ({content; _} as scrollcontainer_info)) ->
                  if content == box then Some scrollcontainer_info else None
              | _ ->
                  None )
            list
        in
        if Option.is_some other_scrollcontainer then begin
          Ui.scrollcontainers :=
            List.filter
              (function
                | ScrollContainer {content; _} ->
                    content != box
                | _ ->
                    failwith "impossible" )
              !Ui.scrollcontainers
        end ;
        let scrollcontainer =
          create_scrollcontainer ~content:box ~orientation
            ~other_scrollcontainer
        in
        let new_list =
          List.map
            (fun (b : box) ->
              let is_nested =
                match b.content with
                | Some (ScrollContainer {content; _}) ->
                    content == box
                | _ ->
                    false
              in
              if b == box || is_nested then
                { Ui.default_box with
                  bbox= box.bbox
                ; content= Some scrollcontainer }
              else b )
            list
        in
        parent.content <- Some (Boxes new_list)
    | _ ->
        (* TODO: handle other_scrollcontainer case *)
        let scrollcontainer =
          create_scrollcontainer ~content:box ~orientation
            ~other_scrollcontainer:None
        in
        parent.content <- Some scrollcontainer
    end
  | None ->
      failwith "cannot wrap box with no contents"
  | _ ->
      failwith ("TODO: " ^ __LOC__)

let unwrap_scrollcontainer ~(box : box) ~unwrap_orientation =
  match box.content with
  | Some
      (ScrollContainer
         ( {content; orientation; other_scrollcontainer; scroll; _} as
           scrollcontainer_info ) ) -> (
      let scroll_bbox = Option.get scroll.bbox in
      let measurement =
        match orientation with
        | Vertical ->
            scroll_bbox.height
        | Horizontal ->
            scroll_bbox.width
      in
      begin if unwrap_orientation = orientation && measurement = 0 then
        match other_scrollcontainer with
        | Some inner_scrollcontainer ->
            box.content <- Some (ScrollContainer inner_scrollcontainer)
        | None -> (
            box.content <- Some (Box content) ;
            match content.bbox with
            | Some bbox ->
                content.bbox <-
                  Some
                    ( match orientation with
                    | Vertical ->
                        { bbox with
                          width= bbox.width + Ui.scrollbar_container_width }
                    | Horizontal ->
                        { bbox with
                          height= bbox.height + Ui.scrollbar_container_width }
                    )
            | None ->
                failwith "SHOULD HAVE BBOX for content.bbox" )
      end ;
      match other_scrollcontainer with
      | Some {orientation; _} when unwrap_orientation = orientation ->
          let scroll_bbox = Option.get scroll.bbox in
          let measurement =
            match orientation with
            | Vertical ->
                scroll_bbox.height
            | Horizontal ->
                scroll_bbox.width
          in
          if measurement = 0 then begin
            box.content <-
              Some
                (ScrollContainer
                   {scrollcontainer_info with other_scrollcontainer= None} ) ;
            match content.bbox with
            | Some bbox ->
                content.bbox <-
                  Some
                    ( match orientation with
                    | Vertical ->
                        { bbox with
                          width= bbox.width + Ui.scrollbar_container_width }
                    | Horizontal ->
                        { bbox with
                          height= bbox.height + Ui.scrollbar_container_width }
                    )
            | None ->
                failwith "SHOULD HAVE bbox for content.bbox"
          end
      | _ ->
          () )
  | _ ->
      ()

let adjust_scrollbar_according_to_content_size ~content ~scroll ~orientation =
  match content.bbox with
  | Some _ -> begin
    let {left; right; top; bottom} = Ui.get_box_sides ~box:content in
    let { bottom= content_bottom
        ; right= content_right
        ; left= content_left
        ; top= content_top } =
      Ui.calculate_content_boundaries ~box:content
    in
    match orientation with
    | Vertical -> begin
      let content_height = content_bottom - content_top
      and parent_height = bottom - top in
      match content_height > parent_height with
      | true -> begin
        match scroll.bbox with
        | Some bbox ->
            let new_scrollbar_height =
              parent_height * parent_height / content_height
            in
            scroll.bbox <-
              Some
                { bbox with
                  height= new_scrollbar_height
                ; y=
                    ( if bbox.y + new_scrollbar_height > bottom then
                        bottom - new_scrollbar_height
                      else bbox.y ) }
        | None ->
            failwith "SHOULD HAVE BBOX for scroll.Ui.bbox"
        end
      | false -> begin
        match scroll.bbox with
        | Some bbox ->
            scroll.bbox <- Some {bbox with y= top; height= 0} ;
            content.scroll_y_offset <- 0
        | None ->
            failwith ("SHOULD HAVE BBOX for scroll.bbox" ^ __LOC__)
        end
      end
    | Horizontal -> begin
      let content_width = content_right - content_left
      and parent_width = right - left in
      match content_width > parent_width with
      | true -> begin
        match scroll.bbox with
        | Some bbox ->
            let new_scrollbar_width =
              parent_width * parent_width / content_width
            in
            scroll.bbox <-
              Some
                { bbox with
                  width= new_scrollbar_width
                ; x=
                    ( if bbox.x + new_scrollbar_width > right then
                        right - new_scrollbar_width
                      else bbox.x ) }
        | None ->
            failwith ("SHOULD HAVE BBOX FOR SCROLL.BBOX" ^ __LOC__)
        end
      | false -> begin
        match scroll.bbox with
        | Some bbox ->
            scroll.bbox <- Some {bbox with x= left; width= 0} ;
            content.scroll_x_offset <- 0
        | None ->
            failwith "SHOULD'VE HAD BBOX"
        end
      end
    end
  | None ->
      ()

let change_content_scroll_offsets_based_off_scrollbar ~content ~scroll
    ~orientation =
  match scroll.bbox with
  | Some scroll_bbox ->
      let {left; right; top; bottom} = Ui.get_box_sides ~box:content in
      let { left= content_left
          ; right= content_right
          ; top= content_top
          ; bottom= content_bottom } =
        Ui.calculate_content_boundaries ~box:content
      in
      let content_width = content_right - content_left in
      let content_height = content_bottom - content_top in
      let content_bbox_width = right - left in
      let content_bbox_height = bottom - top in
      (* distance from scrollbar to start of content divided by content size gives us the percentage
         that is supposed offscreen and I multiply by the content size to get how many pixels should be scrolled.
         negative because it needs to go in the opposite direction of the scrolling *)
      if scroll_bbox.width > 0 && orientation = Horizontal then
        content.scroll_x_offset <-
          -content_width * (scroll_bbox.x - left) / content_bbox_width ;
      if scroll_bbox.height > 0 && orientation = Vertical then
        content.scroll_y_offset <-
          -content_height * (scroll_bbox.y - top) / content_bbox_height
  | _ ->
      ()
