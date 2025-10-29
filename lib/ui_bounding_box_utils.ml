let get_text_bounding_box ~(box : Ui.box) =
  let rope =
    match box.content with
    | Some (Text s) -> Rope.of_string s
    | Some (Textarea { text; _ }) ->
      (try Option.get text with
       | Invalid_argument e -> failwith (__FUNCTION__ ^ "; " ^ e))
    | _ -> failwith __FUNCTION__
  in
  let bbox = Option.value box.bbox ~default:Ui.default_bbox in
  let min_x, min_y, max_x, max_y =
    ref Int.max_int, ref Int.max_int, ref Int.min_int, ref Int.min_int
  in
  let Ui_textarea.{ x; y; _ } =
    Ui_textarea.traverse_rope
      ~rope
      ~handle_result:
        (fun
          (acc : Ui_textarea.rope_traversal_info Ui_textarea.traverse_info) c ->
        let (Rope_Traversal_Info acc) = acc in
        let ~font_info, .. =
          Ui_text_texture_info.get_or_add_font_size_text_texture
            ~font_size:Freetype.font_size
        in
        match c with
        | '\n' ->
          min_y := min !min_y acc.Ui_textarea.y;
          max_y := max !max_y acc.Ui_textarea.y;
          Rope_Traversal_Info
            { y = acc.y + font_info.font_height; x = bbox.x; rope_pos = acc.rope_pos + 1 }
        | _ ->
          min_x := min !min_x acc.Ui_textarea.x;
          max_x := max !max_x acc.Ui_textarea.x;
          let gi = Ui.get_glyph_info_from_glyph ~glyph:c ~font_info in
          let ~new_x, ~new_y, .. =
            Ui.get_text_wrap_info ~bbox ~glyph:c ~x:acc.x ~y:acc.y ~font_info
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

let calculate_bounding_box_of_text_content ~(box : Ui.box) =
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
let rec calculate_content_boundaries ~(box : Ui.box) =
  let Ui.{ left; right; top; bottom } = Ui.get_box_sides ~box in
  match box.content with
  | Some (Box b) ->
    let Ui.{ left = left'; right = right'; top = top'; bottom = bottom' } =
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
           let Ui.{ left; right; top; bottom } = Ui.get_box_sides ~box:b in
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
