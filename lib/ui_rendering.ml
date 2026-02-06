open Freetype
open Ui_types

let _ = Opengl.gl_enable_texture_2d ()
let _ = Opengl.gl_enable_blending ()
let _EACH_POINT_FLOAT_AMOUNT = 6
let _EACH_POINT_FLOAT_AMOUNT_TEXT = 7

type render_buffer_wrapper =
  { buffer : Opengl.render_buffer
  ; mutable length : int
  }

let get_tex_coords
      ~(font_info : Freetype.font_info)
      ~(glyph : char)
      ~(glyph_info : Freetype.glyph_info_)
  =
  let starting_x =
    let acc = ref 0 in
    (* minus 1 because starting_x is summed widths of every glyph before *)
    for idx = 0 to Char.code glyph - 32 - 1 do
      let gi = font_info.glyph_info_with_char.(idx) in
      acc := !acc + gi.width
    done;
    !acc
  in
  let starting_x, ending_x = starting_x, starting_x + glyph_info.width in
  let width_float = Float.of_int font_info.font_texture_atlas.width in
  let height_float = Float.of_int font_info.font_texture_atlas.height in
  let left = Float.of_int starting_x /. width_float in
  let right = Float.of_int ending_x /. width_float in
  left, right, 0., Float.of_int glyph_info.rows /. height_float
;;

let ui_buffer : render_buffer_wrapper =
  { buffer =
      Bigarray.Array1.create
        Bigarray.Float32
        Bigarray.c_layout
        (1000 * 1000 * _EACH_POINT_FLOAT_AMOUNT)
  ; length = 0
  }
;;

let text_buffer : render_buffer_wrapper =
  { buffer =
      Bigarray.Array1.create
        Bigarray.Float32
        Bigarray.c_layout
        (1000 * 1000 * _EACH_POINT_FLOAT_AMOUNT_TEXT)
  ; length = 0
  }
;;

let vertex_cursor =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error e -> failwith (e ^ "_CURSOR")
;;

let fragment_cursor =
  match Opengl.gl_create_fragment_shader () with
  | Ok f -> f
  | Error e -> failwith (e ^ "_CURSOR")
;;

let vertex_highlight =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error e -> failwith e
;;

let fragment_highlight =
  match Opengl.gl_create_fragment_shader () with
  | Ok v -> v
  | Error e -> failwith e
;;

let text_vertex_id =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error e -> failwith (e ^ "; couldn't create vertex shader for text")
;;

let text_vertex_shader =
  {|
  #version 120

  attribute vec2 vertex;
  attribute vec3 color;
  attribute vec2 tex_coord;

  varying vec3 color_frag;
  varying vec2 tex_coord_frag;

  void main() {
    color_frag = color;
    tex_coord_frag = tex_coord;
    gl_Position = vec4(vertex.x, vertex.y, 0.0, 1.0);
  }
  |}
;;

let text_fragment_shader =
  {|
  #version 120

  varying vec2 tex_coord_frag;
  varying vec3 color_frag;

  uniform sampler2D sampler;

  void main() {
    gl_FragColor = vec4(color_frag.r, color_frag.g, color_frag.b, texture2D(sampler, tex_coord_frag).a);
  }
  |}
;;

let generic_vertex_shader =
  {|
  #version 120

  attribute vec2 point_vertex;
  attribute vec4 color_attrib;

  varying vec4 color;

  void main() {
    gl_Position = vec4(point_vertex.x, point_vertex.y, 0.0, 1.0);
    color = color_attrib;
  }
  |}
;;

let generic_fragment_shader =
  {|
  #version 120
  varying vec4 color;
  void main() {
    gl_FragColor = vec4(color.r, color.g, color.b, color.a);
  }
  |}
;;

let vertex_id =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error s -> failwith s
;;

let fragment_id =
  match Opengl.gl_create_fragment_shader () with
  | Ok v -> v
  | Error s -> failwith s
;;

let text_vtx_id =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error s -> failwith s
;;

let text_fragment_id =
  match Opengl.gl_create_fragment_shader () with
  | Ok v -> v
  | Error s -> failwith s
;;

let ui_program =
  Opengl.compile_shaders_and_return_program
    ~vertex_id
    ~fragment_id
    ~vertex_src:generic_vertex_shader
    ~fragment_src:generic_fragment_shader
;;

let location_point_vertex =
  match Opengl.gl_getattriblocation ui_program "point_vertex" with
  | Ok l -> l
  | Error e -> failwith e
;;

let location_color =
  match Opengl.gl_getattriblocation ui_program "color_attrib" with
  | Ok l -> l
  | Error e -> failwith e
;;

let text_shader_program =
  Opengl.compile_shaders_and_return_program
    ~vertex_id:text_vtx_id
    ~fragment_id:text_fragment_id
    ~vertex_src:text_vertex_shader
    ~fragment_src:text_fragment_shader
;;

let vertex_text_location =
  match Opengl.gl_getattriblocation text_shader_program "vertex" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)
;;

let color_text_location =
  match Opengl.gl_getattriblocation text_shader_program "color" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)
;;

let tex_coord_text_location =
  match Opengl.gl_getattriblocation text_shader_program "tex_coord" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)
;;

let sampler_text_location =
  match Opengl.gl_getuniformlocation text_shader_program "sampler" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)
;;

let () =
  Opengl.gl_enable_vertex_attrib_array vertex_text_location;
  Opengl.gl_enable_vertex_attrib_array color_text_location;
  Opengl.gl_enable_vertex_attrib_array tex_coord_text_location;
  Opengl.gl_enable_vertex_attrib_array location_point_vertex;
  Opengl.gl_enable_vertex_attrib_array location_color
;;

let width_ratio, height_ratio = Sdl.get_logical_to_opengl_window_dims_ratio ()

let transform_xy_coords_to_opengl_viewport_coords ~(x : float) ~(y : float) =
  let width_ratio, height_ratio = Float.of_int width_ratio, Float.of_int height_ratio in
  let window_width_height = Sdl.sdl_gl_getdrawablesize () in
  ( (x *. width_ratio /. Float.of_int (window_width_height lsr 32)) -. 1.
  , (-.y *. height_ratio /. Float.of_int (window_width_height land ((1 lsl 32) - 1)))
    +. 1. )
;;

let gl_ui_lib_buffer = Opengl.gl_gen_one_buffer ()

let () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;
  Opengl.gl_buffer_data_big_array
    ~render_buffer:ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim ui_buffer.buffer)
;;

let get_potential_clipped_points ~parent ~points =
  let { left; right; top; bottom } =
    try Ui.get_box_sides ~box:parent with
    | Invalid_argument e -> failwith (e ^ __LOC__)
  in
  let left, right, top, bottom =
    Float.of_int left, Float.of_int right, Float.of_int top, Float.of_int bottom
  in
  let clipped_points = Float.Array.copy points in
  let points_arr_length = Float.Array.length points in
  let idx = ref 0 in
  while !idx < points_arr_length do
    let points_x = Float.Array.get points !idx in
    let points_y = Float.Array.get points (!idx + 1) in
    let clamped_x = Float.max left points_x |> Float.min right in
    let clamped_y = Float.max top points_y |> Float.min bottom in
    Float.Array.set clipped_points !idx clamped_x;
    Float.Array.set clipped_points (!idx + 1) clamped_y;
    idx := !idx + 2
  done;
  clipped_points
;;

let write_container_values_to_ui_buffer ~(box : box) ~(parent : box option) =
  assert (box.bbox <> None);
  let { width; height; x; y; _ } = Option.get box.bbox
  and r, g, b, alpha = box.background_color in
  let points : floatarray =
    [| Float.of_int x
     ; Float.of_int (y + height)
     ; Float.of_int x
     ; Float.of_int y
     ; Float.of_int (x + width)
     ; Float.of_int y
     ; Float.of_int (x + width)
     ; Float.of_int (y + height)
    |]
  in
  let points =
    match parent with
    | Some parent when parent.clip_content -> get_potential_clipped_points ~parent ~points
    | _ -> points
  in
  let idx = ref ui_buffer.length in
  let float_array_index = ref 0 in
  while !float_array_index < Float.Array.length points do
    let x = Float.Array.get points !float_array_index
    and y = Float.Array.get points (!float_array_index + 1) in
    let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
    ui_buffer.buffer.{!idx} <- x;
    ui_buffer.buffer.{!idx + 1} <- y;
    ui_buffer.buffer.{!idx + 2} <- r;
    ui_buffer.buffer.{!idx + 3} <- g;
    ui_buffer.buffer.{!idx + 4} <- b;
    ui_buffer.buffer.{!idx + 5} <- alpha;
    idx := !idx + 6;
    float_array_index := !float_array_index + 2
  done;
  ui_buffer.length <- !idx
;;

let gl_text_buffer = Opengl.gl_gen_one_buffer ()

let () =
  Opengl.gl_bind_buffer gl_text_buffer;
  Opengl.gl_buffer_data_big_array
    ~render_buffer:text_buffer.buffer
    ~capacity:(Bigarray.Array1.dim text_buffer.buffer)
;;

let get_new_tex_coords_based_off_of_clipped_points
      ~clipped_points
      ~points
      ~(glyph_info : Freetype.glyph_info_)
      (left, right, top, bottom)
  =
  (*
  what it is (corner of vertex); index
  left bottom x;0
  left bottom y;1
  left top x;2
  left top y;3
  right top x;4
  right top y;5
  right bottom x;6
  right bottom y;7
  *)
  let get_values_from_index_for_both i =
    Float.Array.get points i, Float.Array.get clipped_points i
  in
  let glyph_width = Float.of_int glyph_info.Freetype.width
  and glyph_height = Float.of_int glyph_info.Freetype.rows
  and points_x_left, clipped_x_left = get_values_from_index_for_both 0 in
  let diff_x_left = (clipped_x_left -. points_x_left) /. glyph_width
  and points_y_bottom, clipped_y_bottom = get_values_from_index_for_both 1 in
  let diff_y_bottom = (clipped_y_bottom -. points_y_bottom) /. glyph_height
  and points_x_right, clipped_x_right = get_values_from_index_for_both 4 in
  let diff_x_right = (clipped_x_right -. points_x_right) /. glyph_width
  and points_y_top, clipped_y_top = get_values_from_index_for_both 5 in
  let diff_y_top = (clipped_y_top -. points_y_top) /. glyph_height
  and glyph_texture_width = right -. left
  and glyph_texture_height = bottom -. top in
  ( left +. (diff_x_left *. glyph_texture_width)
  , right +. (diff_x_right *. glyph_texture_width)
  , top +. (diff_y_top *. glyph_texture_height)
  , bottom +. (diff_y_bottom *. glyph_texture_height) )
;;

let write_to_text_buffer
      ~(glyph_info : Freetype.glyph_info_)
      ~x
      ~y
      ~(glyph : char)
      ~(font_info : Freetype.font_info)
      ~parent
  =
  (*
    left bottom x;
    left bottom y;
    left top x;
    left top y;
    right top x;
    right top y;
    right bottom x;
    right bottom y;
    *)
  let { left; right; top; bottom } = Ui.get_box_sides ~box:parent in
  let glyph_left, glyph_right, glyph_top, glyph_bottom =
    ( x + glyph_info.horiBearingX
    , x + glyph_info.width + glyph_info.horiBearingX
    , y - glyph_info.horiBearingY
    , y + glyph_info.rows - glyph_info.horiBearingY )
  in
  let out =
    parent.clip_content
    && (glyph_right < left
        || glyph_left > right
        || glyph_bottom < top
        || glyph_top > bottom)
  in
  if not out
  then (
    let points : floatarray =
      [| Float.of_int glyph_left
       ; Float.of_int glyph_bottom
       ; Float.of_int glyph_left
       ; Float.of_int glyph_top
       ; Float.of_int glyph_right
       ; Float.of_int glyph_top
       ; Float.of_int glyph_right
       ; Float.of_int glyph_bottom
      |]
    in
    let clipped_points =
      if
        parent.clip_content
        && (glyph_right > right
            || glyph_left < left
            || glyph_top < top
            || glyph_bottom > bottom)
      then get_potential_clipped_points ~parent ~points
      else points
    in
    let left, right, top, bottom = get_tex_coords ~font_info ~glyph ~glyph_info in
    let left, right, top, bottom =
      get_new_tex_coords_based_off_of_clipped_points
        ~clipped_points
        ~points
        ~glyph_info
        (left, right, top, bottom)
    in
    let points = clipped_points in
    let points_idx = ref 0 in
    while !points_idx < Float.Array.length points do
      let x = Float.Array.get points !points_idx
      and y = Float.Array.get points (!points_idx + 1) in
      let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
      Float.Array.set points !points_idx x;
      Float.Array.set points (!points_idx + 1) y;
      points_idx := !points_idx + 2
    done;
    (*
     layout of the values list is:
       vertex x
       vertex y
       r
       g
       b
       texel coord x
       texel coord y

      that is repeated for the 4 points of the quad
   *)
    let values : floatarray =
      [| Float.Array.get points 0
       ; Float.Array.get points 1
       ; 0.
       ; 0.
       ; 0.
       ; left
       ; bottom
       ; Float.Array.get points 2
       ; Float.Array.get points 3
       ; 0.
       ; 0.
       ; 0.
       ; left
       ; top
       ; Float.Array.get points 4
       ; Float.Array.get points 5
       ; 0.
       ; 0.
       ; 0.
       ; right
       ; top
       ; Float.Array.get points 6
       ; Float.Array.get points 7
       ; 0.
       ; 0.
       ; 0.
       ; right
       ; bottom
      |]
    in
    let start = text_buffer.length in
    Float.Array.iteri (fun idx v -> text_buffer.buffer.{idx + start} <- v) values;
    text_buffer.length <- start + Float.Array.length values)
;;

let draw_to_gl_buffer_text () =
  Opengl.gl_bind_buffer gl_text_buffer;
  Opengl.gl_use_program text_shader_program;
  Opengl.gl_uniform_1i ~location:sampler_text_location ~value:0;
  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:vertex_text_location
    ~size:2
    ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT
    ~normalized:false
    ~start_idx:0;
  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:color_text_location
    ~size:3
    ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT
    ~normalized:false
    ~start_idx:2;
  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:tex_coord_text_location
    ~size:2
    ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT
    ~normalized:false
    ~start_idx:5;
  Opengl.gl_buffer_subdata_big_array
    ~render_buffer:text_buffer.buffer
    ~length:text_buffer.length;
  Opengl.gl_draw_arrays_with_quads (text_buffer.length / _EACH_POINT_FLOAT_AMOUNT_TEXT);
  text_buffer.length <- 0
;;

let draw_to_gl_buffer () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;
  Opengl.gl_use_program ui_program;
  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:location_point_vertex
    ~size:2
    ~stride:_EACH_POINT_FLOAT_AMOUNT
    ~normalized:false
    ~start_idx:0;
  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:location_color
    ~size:4
    ~stride:_EACH_POINT_FLOAT_AMOUNT
    ~normalized:false
    ~start_idx:2;
  Opengl.gl_buffer_subdata_big_array
    ~render_buffer:ui_buffer.buffer
    ~length:ui_buffer.length;
  Opengl.gl_draw_arrays_with_quads (ui_buffer.length / _EACH_POINT_FLOAT_AMOUNT);
  ui_buffer.length <- 0
;;

let get_vertical_text_start ~(box : box) ~(font_info : Freetype.font_info) =
  try
    let bbox = Option.get box.bbox in
    let start_of_vertical =
      match box.vertical_align with
      | Some Top | None -> bbox.y
      | Some Center ->
        let start = bbox.y + (bbox.height / 2) - (font_info.font_height / 2) in
        start
      | Some Bottom -> bbox.y + bbox.height - font_info.font_height
    in
    start_of_vertical + font_info.ascender
  with
  | Invalid_argument e -> failwith ("alignment requires a bbox;" ^ e ^ __LOC__)
;;

let get_horizontal_text_start ~(box : box) ~(font_info : Freetype.font_info) ~(s : string)
  =
  let width_of_string =
    String.fold_left
      (fun acc c ->
         let glyph = font_info.glyph_info_with_char.(Char.code c - 32) in
         acc + glyph.x_advance)
      0
      s
  in
  try
    let bbox = Option.get box.bbox in
    match box.horizontal_align with
    | Some Left | None -> bbox.x
    | Some Center -> bbox.x + (bbox.width / 2) - (width_of_string / 2)
    | Some Right -> bbox.x + bbox.width - width_of_string
  with
  | Invalid_argument e -> failwith ("alignment requires a bbox;" ^ e ^ __LOC__)
;;

let write_highlight_to_ui_buffer ~(points : int list) ~parent =
  let points = points |> List.map Float.of_int |> Float.Array.of_list in
  let points =
    match parent with
    | Some parent -> get_potential_clipped_points ~parent ~points
    | None -> points
  in
  let idx = ref 0 in
  let ui_buffer_idx = ref ui_buffer.length in
  while !idx < Float.Array.length points do
    let x, y = Float.Array.get points !idx, Float.Array.get points (!idx + 1) in
    let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
    ui_buffer.buffer.{!ui_buffer_idx} <- x;
    ui_buffer.buffer.{!ui_buffer_idx + 1} <- y;
    ui_buffer.buffer.{!ui_buffer_idx + 2} <- 0.;
    ui_buffer.buffer.{!ui_buffer_idx + 3} <- 0.;
    ui_buffer.buffer.{!ui_buffer_idx + 4} <- 1.;
    ui_buffer.buffer.{!ui_buffer_idx + 5} <- 0.5;
    idx := !idx + 2;
    ui_buffer_idx := !ui_buffer_idx + _EACH_POINT_FLOAT_AMOUNT
  done;
  ui_buffer.length <- !ui_buffer_idx
;;

let draw_highlight
      ~(box : box)
      ~(font_info : Freetype.font_info)
      ~(r : Rope_types.rope)
      ~(highlight : int option * int option)
  =
  let bbox = Option.value box.bbox ~default:Ui.default_bbox in
  let entire_points_of_highlight_quads = ref [] in
  match highlight with
  | Some highlight_start, Some highlight_end ->
    let fn_for_draw_highlight acc c =
      let (Rope_types.Rope_Traversal_Info acc) = acc in
      match c with
      | '\n' -> ()
      | _ ->
        let gi = Freetype.get_glyph_info_from_glyph ~glyph:c ~font_info in
        let x_advance = gi.x_advance in
        let next_y = acc.y + font_info.font_height in
        let ~wraps, .. =
          Ui_utils.get_text_wrap_info ~box ~glyph:c ~x:acc.x ~y:acc.y ~font_info
        in
        if acc.rope_pos >= highlight_start && acc.rope_pos < highlight_end
        then (
          let points =
            [ acc.x + box.scroll_x_offset
            ; (next_y + if wraps then font_info.font_height else 0)
            ; acc.x + box.scroll_x_offset
            ; (acc.y + if wraps then font_info.font_height else 0)
            ; acc.x + box.scroll_x_offset + x_advance
            ; (acc.y + if wraps then font_info.font_height else 0)
            ; acc.x + box.scroll_x_offset + x_advance
            ; (next_y + if wraps then font_info.font_height else 0)
            ]
          in
          entire_points_of_highlight_quads
          := List.append points !entire_points_of_highlight_quads)
    in
    ignore
      (Rope.traverse_rope
         ~box
         ~font_info
         ~rope:r
         ~handle_result:(Some fn_for_draw_highlight)
         ~result:
           (Rope_types.Rope_Traversal_Info
              { x = bbox.x; y = bbox.y + box.scroll_y_offset; rope_pos = 0 }));
    write_highlight_to_ui_buffer
      ~points:!entire_points_of_highlight_quads
      ~parent:(Some box)
  | _ -> ()
;;

let write_cursor_to_ui_buffer ~parent ~x ~y ~font_height =
  let points =
    [ x
    ; y + font_height
    ; x
    ; y
    ; x + Ui.text_caret_width
    ; y
    ; x + Ui.text_caret_width
    ; y + font_height
    ]
    |> List.map Float.of_int
    |> Float.Array.of_list
  in
  let points =
    match parent with
    | Some parent -> get_potential_clipped_points ~parent ~points
    | None -> points
  in
  let idx = ref 0 in
  let ui_buffer_idx = ref ui_buffer.length in
  while !idx < Float.Array.length points do
    let x, y = Float.Array.get points !idx, Float.Array.get points (!idx + 1) in
    let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
    ui_buffer.buffer.{!ui_buffer_idx} <- x;
    ui_buffer.buffer.{!ui_buffer_idx + 1} <- y;
    ui_buffer.buffer.{!ui_buffer_idx + 2} <- 0.;
    ui_buffer.buffer.{!ui_buffer_idx + 3} <- 0.;
    ui_buffer.buffer.{!ui_buffer_idx + 4} <- 0.;
    ui_buffer.buffer.{!ui_buffer_idx + 5} <- 1.;
    idx := !idx + 2;
    ui_buffer_idx := !ui_buffer_idx + _EACH_POINT_FLOAT_AMOUNT
  done;
  ui_buffer.length <- !ui_buffer_idx
;;

let validate ~(box : box) =
  let rec validate' (box : box) visited =
    if List.exists (fun b -> b == box) visited
    then failwith "Recursive box structure detected"
    else (
      match box.content with
      | Some (Box b) -> validate' b (box :: visited)
      | Some (Boxes list) ->
        let visited = box :: visited in
        List.iter (fun b -> validate' b visited) list
      | Some (Text _) -> ()
      | Some (Textarea _) -> ()
      | None -> ())
  in
  validate' box []
;;

let add_event_handlers ~(box : box) =
  let rec add_event_handlers' (box : box) =
    (match box.on_event with
     | Some oc -> Ui_events.add_event_handler ~box:(Some box) ~event_handler:oc
     | None -> Ui_events.remove_event_handler ~box);
    match box.content with
    | Some (Box b) -> add_event_handlers' b
    | Some (Boxes list) -> List.iter (fun b -> add_event_handlers' b) list
    | Some (Text _) | Some (Textarea _) | None -> ()
  in
  add_event_handlers' box
;;

let handle_list_of_boxes_initial_position ~(box : box) ~(d : direction) ~(list : box list)
  =
  assert (Option.is_some box.bbox);
  let box_bbox = Option.get box.bbox in
  let acc_width, acc_height =
    List.fold_left
      (fun (acc_w, acc_h) b ->
         let bbox = Option.value b.bbox ~default:Ui.default_bbox in
         match d with
         | Horizontal -> acc_w + bbox.width, bbox.height
         | Vertical -> bbox.width, acc_h + bbox.height)
      (0, 0)
      list
  in
  let box_bbox_used_width, box_bbox_used_height = box_bbox.width, box_bbox.height in
  let x_pos =
    match box.horizontal_align with
    | Some Left | None -> box_bbox.x
    | Some Center -> box_bbox.x + (box_bbox_used_width / 2) - (acc_width / 2)
    | Some Right -> box_bbox.x + box_bbox_used_width - acc_width
  in
  let y_pos =
    match box.vertical_align with
    | Some Top | None -> box_bbox.y
    | Some Center -> box_bbox.y + (box_bbox_used_height / 2) - (acc_height / 2)
    | Some Bottom -> box_bbox.y + box_bbox_used_height - acc_height
  in
  x_pos + box.scroll_x_offset, y_pos + box.scroll_y_offset
;;

let align_inner_box_vertically ~(box : box) ~(inner_box : box) =
  match box.bbox with
  | Some bbox ->
    let inner_box_bbox = Option.value inner_box.bbox ~default:Ui.default_bbox in
    let box_used_height = bbox.height in
    let relative_y =
      match inner_box.position_type with
      | Relative { y; _ } -> y
      | _ -> 0
    in
    (match box.vertical_align with
     | Some Top ->
       let y_pos = bbox.y + relative_y in
       inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
     | Some Center ->
       let y_pos =
         bbox.y + (box_used_height / 2) - (inner_box_bbox.height / 2) + relative_y
       in
       inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
     | Some Bottom ->
       let y_pos = bbox.y + box_used_height - inner_box_bbox.height + relative_y in
       inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
     | None -> ())
  | None -> ()
;;

let align_inner_box_horizontally ~(box : box) ~(inner_box : box) =
  match box.bbox with
  | Some bbox ->
    let inner_box_bbox = Option.value inner_box.bbox ~default:Ui.default_bbox in
    let box_used_width = bbox.width in
    let relative_x =
      match inner_box.position_type with
      | Relative { x; _ } -> x
      | _ -> 0
    in
    (match box.horizontal_align with
     | Some Left ->
       let x_pos = bbox.x + relative_x in
       inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
     | Some Center ->
       let x_pos =
         bbox.x + (box_used_width / 2) - (inner_box_bbox.width / 2) + relative_x
       in
       inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
     | Some Right ->
       let x_pos = bbox.x + box_used_width - inner_box_bbox.width + relative_x in
       inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
     | None -> ())
  | None -> ()
;;

let draw_cursor
      ~(font_info : Freetype.font_info)
      ~(box : box)
      ~(r : Rope_types.rope)
      ~cursor_pos
  =
  assert (box.bbox <> None);
  let bbox = Option.get box.bbox in
  match cursor_pos with
  | Some cursor_pos ->
    let fn_draw_cursor acc _c =
      let (Rope_types.Rope_Traversal_Info acc) = acc in
      let y_pos = acc.y in
      let x_pos = acc.x + box.scroll_x_offset in
      if
        acc.rope_pos = cursor_pos
        && y_pos >= bbox.y
        && y_pos <= bbox.y + bbox.height
        && x_pos >= bbox.x
        && x_pos <= bbox.x + bbox.width
      then
        write_cursor_to_ui_buffer
          ~x:x_pos
          ~y:acc.y
          ~font_height:font_info.font_height
          ~parent:(Some box)
    in
    let res =
      Rope.traverse_rope
        ~box
        ~font_info
        ~rope:r
        ~handle_result:(Some fn_draw_cursor)
        ~result:
          (Rope_types.Rope_Traversal_Info
             { x = bbox.x; y = bbox.y + box.scroll_y_offset; rope_pos = 0 })
    in
    if res.rope_pos = cursor_pos
    then
      write_cursor_to_ui_buffer
        ~x:(res.x + box.scroll_x_offset)
        ~y:res.y
        ~font_height:font_info.font_height
        ~parent:(Some box)
  | None -> ()
;;

type draw_context =
  { parent : box option
  ; previous_context : draw_context option
  }

let find_closest_parent_that_clips ~(context : draw_context) ~bbox =
  let rec loop context =
    match context.parent with
    | Some parent ->
      let { left; right; top; bottom } = Ui.get_box_sides ~box:parent in
      let bbox_left, bbox_right, bbox_top, bbox_bottom =
        bbox.x, bbox.x + bbox.width, bbox.y, bbox.y + bbox.height
      in
      let out =
        bbox_left > right
        || bbox_right < left
        || bbox_left < left
        || bbox_right > right
        || bbox_top > bottom
        || bbox_bottom < top
        || bbox_top < top
        || bbox_bottom > bottom
      in
      if (parent.clip_content && out) || not out
      then Some parent
      else (
        match context.previous_context with
        | Some context -> loop context
        | None -> None)
    | None -> None
  in
  loop context
;;

let draw_text ~(s : string) ~(box : box) ~context =
  let ~font_info, ~gl_texture_id =
    Ui.TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
  in
  Opengl.gl_bind_texture ~texture_id:gl_texture_id;
  let start_y = get_vertical_text_start ~box ~font_info + box.scroll_y_offset in
  let start_x = get_horizontal_text_start ~box ~font_info ~s + box.scroll_x_offset in
  let horizontal_pos = ref start_x in
  let string_length = Ui.calculate_string_width ~s ~font_info in
  let parent =
    find_closest_parent_that_clips
      ~context
      ~bbox:
        { x = start_x
        ; y = start_y
        ; width = string_length
        ; height = font_info.font_height
        }
  in
  let parent = Option.value parent ~default:box in
  String.iter
    (fun c ->
       let glyph = font_info.glyph_info_with_char.(Char.code c - 32) in
       write_to_text_buffer
         ~glyph_info:glyph
         ~x:!horizontal_pos
         ~y:start_y
         ~glyph:c
         ~font_info
         ~parent;
       horizontal_pos := !horizontal_pos + glyph.x_advance)
    s
;;

let draw_text_textarea
      ~(font_info : Freetype.font_info)
      ~(box : box)
      ~(rope : Rope_types.rope)
  =
  assert (box.bbox <> None);
  let bbox = Option.get box.bbox in
  let fn_for_drawing_text (Rope_types.Rope_Traversal_Info acc) c =
    if c <> '\n'
    then (
      let gi = Freetype.get_glyph_info_from_glyph ~glyph:c ~font_info in
      let ~wraps, .. =
        Ui_utils.get_text_wrap_info ~box ~glyph:c ~x:acc.x ~y:acc.y ~font_info
      in
      (* descender is a negative value *)
      let descender = font_info.descender in
      let y_pos_start = acc.y + descender + if wraps then font_info.font_height else 0 in
      if
        y_pos_start <= bbox.y + bbox.height
        && y_pos_start >= bbox.y
        && Bytes.length gi.bytes > 0
      then
        write_to_text_buffer
          ~x:(acc.x + box.scroll_x_offset)
          ~y:y_pos_start
          ~glyph_info:gi
          ~glyph:c
          ~font_info
          ~parent:box)
  in
  ignore
    (Rope.traverse_rope
       ~box
       ~font_info
       ~rope
       ~handle_result:(Some fn_for_drawing_text)
       ~result:
         (Rope_types.Rope_Traversal_Info
            { rope_pos = 0
            ; x = bbox.x
            ; y = bbox.y + font_info.font_height + box.scroll_y_offset
            }))
;;

let draw_textarea
      ~(font_info : Freetype.font_info)
      ~rope
      ~highlight
      ~cursor_pos
      ~(box : box)
  =
  match rope with
  | Some r ->
    draw_text_textarea ~font_info ~rope:r ~box;
    (match !Ui_globals.focused_element with
     | Some b when b == box ->
       draw_highlight ~r ~highlight ~box ~font_info;
       draw_cursor ~r ~cursor_pos ~font_info ~box
     | _ -> ())
  | None -> ()
;;

let rec draw_box ~(box : box) ~(context : draw_context) =
  match box.bbox with
  | Some bbox ->
    let { left; top; bottom; right } = Ui.get_box_sides ~box in
    let window_width_height = Sdl.sdl_gl_getdrawablesize () in
    let window_width_gl, window_height_gl =
      window_width_height lsr 32, window_width_height land ((1 lsl 32) - 1)
    in
    if left <= window_width_gl && right >= 0 && top <= window_height_gl && bottom >= 0
    then (
      let parent = find_closest_parent_that_clips ~context ~bbox in
      let scrollcontainer =
        List.find_opt (fun (b', _) -> b' == box) !Ui_globals.scrollcontainers
      in
      (match scrollcontainer with
       | Some (_, scrollcontainer_info) -> render_scrollcontainer ~scrollcontainer_info
       | None -> ());
      write_container_values_to_ui_buffer ~box ~parent;
      match box.content with
      | Some (Box b) ->
        draw_box ~box:b ~context:{ parent = Some box; previous_context = Some context }
      | Some (Boxes list) ->
        List.iter
          (fun b ->
             draw_box
               ~box:b
               ~context:{ parent = Some box; previous_context = Some context })
          list
      | Some (Text { string; _ }) ->
        draw_text
          ~s:string
          ~box
          ~context:{ parent = Some box; previous_context = Some context }
      | Some (Textarea { text; cursor_pos; highlight_pos; _ }) ->
        let ~font_info, ~gl_texture_id =
          Ui.TextTextureInfo.get_or_add_font_size_text_texture
            ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
        in
        Opengl.gl_bind_texture ~texture_id:gl_texture_id;
        draw_textarea ~rope:text ~cursor_pos ~highlight:highlight_pos ~font_info ~box
      | None -> ())
  | None -> ()

and handle_if_content_overflows_or_not ~(box : box) ~(context : ui_traversal_context) =
  let { left = content_left
      ; right = content_right
      ; top = content_top
      ; bottom = content_bottom
      }
    =
    Ui.calculate_content_boundaries ~box
  in
  assert (box.bbox <> None);
  let ({ width; height; _ } as _bbox) = Option.get box.bbox in
  if not context.in_scrollcontainer
  then (
    let has_horizontal_scroll_info =
      List.exists
        (fun (box', { horizontal_scroll_info; _ }) ->
           box' == box && Option.is_some horizontal_scroll_info)
        !Ui_globals.scrollcontainers
    in
    let has_vertical_scroll_info =
      List.exists
        (fun (box', { vertical_scroll_info; _ }) ->
           box' == box && Option.is_some vertical_scroll_info)
        !Ui_globals.scrollcontainers
    in
    let already_existing_or_default =
      List.find_map
        (fun (box', si) -> if box' == box then Some si else None)
        !Ui_globals.scrollcontainers
      |> fun o ->
      Option.value
        o
        ~default:{ vertical_scroll_info = None; horizontal_scroll_info = None }
      |> ref
    in
    if
      content_right - content_left > width
      && box.allow_horizontal_scroll
      && not has_horizontal_scroll_info
    then (
      let new_scrollcontainer =
        Ui_scrollcontainers.create_horizontal_scrollcontainer ~content:box
      in
      already_existing_or_default
      := { !already_existing_or_default with
           horizontal_scroll_info = Some new_scrollcontainer
         });
    if
      content_bottom - content_top > height
      && box.allow_vertical_scroll
      && not has_vertical_scroll_info
    then (
      let new_scrollcontainer =
        Ui_scrollcontainers.create_vertical_scrollcontainer ~content:box
      in
      already_existing_or_default
      := { !already_existing_or_default with
           vertical_scroll_info = Some new_scrollcontainer
         });
    if
      (not has_horizontal_scroll_info)
      && (not has_vertical_scroll_info)
      && (!already_existing_or_default.vertical_scroll_info |> Option.is_some
          || !already_existing_or_default.horizontal_scroll_info |> Option.is_some)
    then
      Ui_globals.scrollcontainers
      := (box, !already_existing_or_default) :: !Ui_globals.scrollcontainers)

and adjust_scrollcontainer_if_needed ~(box : box) =
  let scrollcontainer_info =
    List.find_opt (fun (box', _) -> box' == box) !Ui_globals.scrollcontainers
  in
  match scrollcontainer_info with
  | Some (_, scrollcontainer_info) ->
    Ui_scrollcontainers.adjust_scrollbar_container_according_to_content_size
      ~scrollcontainer_info
      ~content:box;
    Ui_scrollcontainers.change_content_scroll_offsets_based_off_scrollbar
      ~scrollcontainer_info
      ~content:box
  | None -> ()

and calculate_ui_for_scrollcontainer ~(scrollcontainer_info : scrollcontainer_info) =
  let { vertical_scroll_info; horizontal_scroll_info } = scrollcontainer_info in
  Option.iter
    (fun { vertical_scrollbar_container; _ } ->
       calculate_ui
         ~box:vertical_scrollbar_container
         ~context:{ in_scrollcontainer = false; parent = None })
    vertical_scroll_info;
  Option.iter
    (fun { horizontal_scrollbar_container; _ } ->
       calculate_ui
         ~box:horizontal_scrollbar_container
         ~context:{ in_scrollcontainer = false; parent = None })
    horizontal_scroll_info

and calculate_ui ~(box : box) ~context =
  Option.iter (fun update -> update ()) box.update;
  Ui.constrain_width_height ~box ~context;
  handle_if_content_overflows_or_not ~box ~context;
  assert (Option.is_some box.bbox);
  let scrollcontainer =
    List.find_opt (fun (b', _) -> b' == box) !Ui_globals.scrollcontainers
  in
  (match scrollcontainer with
   | Some (_, scrollcontainer_info) ->
     calculate_ui_for_scrollcontainer ~scrollcontainer_info;
     adjust_scrollcontainer_if_needed ~box
   | None -> ());
  match box.content with
  | Some (Box b) ->
    let parent_bbox = Option.get box.bbox in
    let bbox = Option.value b.bbox ~default:Ui.default_bbox in
    if not (Ui.is_within_box ~x:bbox.x ~y:bbox.y ~from_sdl_evt:false ~box)
    then b.bbox <- Some { bbox with x = parent_bbox.x; y = parent_bbox.y };
    align_inner_box_horizontally ~box ~inner_box:b;
    align_inner_box_vertically ~box ~inner_box:b;
    calculate_ui ~box:b ~context:{ context with parent = Some box }
  | Some (Boxes list) ->
    (match box.flow with
     | Some ((Horizontal | Vertical) as d) ->
       let boxes_pos = ref (handle_list_of_boxes_initial_position ~d ~box ~list) in
       list
       |> List.iter (fun b ->
         match b.bbox with
         | Some bbbox ->
           bbbox.x <- fst !boxes_pos;
           bbbox.y <- snd !boxes_pos;
           let bbbox_used_width, bbbox_used_height = bbbox.width, bbbox.height in
           (boxes_pos
            := let x, y = !boxes_pos in
               match d with
               | Horizontal -> x + bbbox_used_width, y
               | Vertical -> x, y + bbbox_used_height);
           calculate_ui ~box:b ~context:{ context with parent = Some box }
         | None -> ())
     | None -> ())
  | Some (Text _) -> ()
  | Some (Textarea _) -> ()
  | None -> ()

and render_scrollcontainer ~(scrollcontainer_info : scrollcontainer_info) =
  let { vertical_scroll_info; horizontal_scroll_info } = scrollcontainer_info in
  Option.iter
    (fun { vertical_scrollbar_container; _ } ->
       draw_box
         ~box:vertical_scrollbar_container
         ~context:{ parent = None; previous_context = None })
    vertical_scroll_info;
  Option.iter
    (fun { horizontal_scrollbar_container; _ } ->
       draw_box
         ~box:horizontal_scrollbar_container
         ~context:{ parent = None; previous_context = None })
    horizontal_scroll_info
;;

let draw ~(box : box) =
  Opengl.gl_clear_color 1. 1. 1. 1.;
  Opengl.gl_clear ();
  validate ~box;
  add_event_handlers ~box;
  calculate_ui ~box ~context:{ in_scrollcontainer = false; parent = None };
  draw_box ~box ~context:{ parent = None; previous_context = None };
  draw_to_gl_buffer ();
  draw_to_gl_buffer_text ();
  Sdl.sdl_gl_swapwindow Sdl.w
;;
