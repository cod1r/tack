open Freetype

let _ = Opengl.gl_enable_texture_2d ()
let _ = Opengl.gl_enable_blending ()
let _EACH_POINT_FLOAT_AMOUNT = 6
let _EACH_POINT_FLOAT_AMOUNT_TEXT = 7

type render_buffer_wrapper = {
  buffer : Opengl.render_buffer;
  mutable length : int;
}

let get_tex_coords ~(font_info : Freetype.font_info) ~(glyph : char)
    ~(glyph_info : Freetype.glyph_info_) =
  let starting_x =
    let acc = ref 0 in
    (* minus 1 because starting_x is summed widths of every glyph before *)
    for idx = 0 to Char.code glyph - 32 - 1 do
      let gi = font_info.glyph_info_with_char.(idx) in
      acc := !acc + gi.width
    done;
    !acc
  in
  let starting_x, ending_x = (starting_x, starting_x + glyph_info.width) in
  let width_float = Float.of_int font_info.font_texture_atlas.width in
  let height_float = Float.of_int font_info.font_texture_atlas.height in
  let left = Float.of_int starting_x /. width_float in
  let right = Float.of_int ending_x /. width_float in
  (left, right, 0., Float.of_int glyph_info.rows /. height_float)

let ui_buffer : render_buffer_wrapper =
  {
    buffer =
      Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout
        (1000 * 1000 * _EACH_POINT_FLOAT_AMOUNT);
    length = 0;
  }

let text_buffer : render_buffer_wrapper =
  {
    buffer =
      Bigarray.Array1.create Bigarray.Float32 Bigarray.c_layout
        (1000 * 1000 * _EACH_POINT_FLOAT_AMOUNT_TEXT);
    length = 0;
  }

let vertex_cursor =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error e -> failwith (e ^ "_CURSOR")

let fragment_cursor =
  match Opengl.gl_create_fragment_shader () with
  | Ok f -> f
  | Error e -> failwith (e ^ "_CURSOR")

let vertex_highlight =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error e -> failwith e

let fragment_highlight =
  match Opengl.gl_create_fragment_shader () with
  | Ok v -> v
  | Error e -> failwith e

let text_vertex_id =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error e -> failwith (e ^ "; couldn't create vertex shader for text")

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

let generic_fragment_shader =
  {|
  #version 120
  varying vec4 color;
  void main() {
    gl_FragColor = vec4(color.r, color.g, color.b, color.a);
  }
  |}

let vertex_id =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error s -> failwith s

let fragment_id =
  match Opengl.gl_create_fragment_shader () with
  | Ok v -> v
  | Error s -> failwith s

let text_vtx_id =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error s -> failwith s

let text_fragment_id =
  match Opengl.gl_create_fragment_shader () with
  | Ok v -> v
  | Error s -> failwith s

let ui_program =
  Opengl.compile_shaders_and_return_program ~vertex_id ~fragment_id
    ~vertex_src:generic_vertex_shader ~fragment_src:generic_fragment_shader

let location_point_vertex =
  match Opengl.gl_getattriblocation ui_program "point_vertex" with
  | Ok l -> l
  | Error e -> failwith e

let location_color =
  match Opengl.gl_getattriblocation ui_program "color_attrib" with
  | Ok l -> l
  | Error e -> failwith e

let text_shader_program =
  Opengl.compile_shaders_and_return_program ~vertex_id:text_vtx_id
    ~fragment_id:text_fragment_id ~vertex_src:text_vertex_shader
    ~fragment_src:text_fragment_shader

let vertex_text_location =
  match Opengl.gl_getattriblocation text_shader_program "vertex" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

let color_text_location =
  match Opengl.gl_getattriblocation text_shader_program "color" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

let tex_coord_text_location =
  match Opengl.gl_getattriblocation text_shader_program "tex_coord" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

let sampler_text_location =
  match Opengl.gl_getuniformlocation text_shader_program "sampler" with
  | Ok l -> l
  | Error e -> failwith (e ^ " " ^ __FILE__ ^ " " ^ string_of_int __LINE__)

let () =
  Opengl.gl_enable_vertex_attrib_array vertex_text_location;
  Opengl.gl_enable_vertex_attrib_array color_text_location;
  Opengl.gl_enable_vertex_attrib_array tex_coord_text_location;
  Opengl.gl_enable_vertex_attrib_array location_point_vertex;
  Opengl.gl_enable_vertex_attrib_array location_color

let width_ratio, height_ratio = Sdl.get_logical_to_opengl_window_dims_ratio ()

let transform_xy_coords_to_opengl_viewport_coords ~(x : float) ~(y : float) =
  let width_ratio, height_ratio =
    (Float.of_int width_ratio, Float.of_int height_ratio)
  in
  let window_width_height = Sdl.sdl_gl_getdrawablesize () in
  ( (x *. width_ratio /. Float.of_int (window_width_height lsr 32)) -. 1.,
    -.y *. height_ratio
    /. Float.of_int (window_width_height land ((1 lsl 32) - 1))
    +. 1. )

let gl_ui_lib_buffer = Opengl.gl_gen_one_buffer ()

let () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;
  Opengl.gl_buffer_data_big_array ~render_buffer:ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim ui_buffer.buffer)

let write_container_values_to_ui_buffer ~(box : Ui.box) =
  let Ui.{ width; height; x; y; _ } =
    Option.value box.bbox ~default:Ui.default_bbox
  and r, g, b, alpha = box.background_color in
  let points : floatarray =
    [|
      Float.of_int x;
      Float.of_int (y + height);
      Float.of_int x;
      Float.of_int y;
      Float.of_int (x + width);
      Float.of_int y;
      Float.of_int (x + width);
      Float.of_int (y + height);
    |]
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
  ui_buffer.length <- ui_buffer.length + 24

let gl_text_buffer = Opengl.gl_gen_one_buffer ()

let () =
  Opengl.gl_bind_buffer gl_text_buffer;
  Opengl.gl_buffer_data_big_array ~render_buffer:text_buffer.buffer
    ~capacity:(Bigarray.Array1.dim text_buffer.buffer)

let write_to_text_buffer ~(glyph_info : Freetype.glyph_info_) ~x ~y
    ~(glyph : char) ~(font_info : Freetype.font_info) =
  let points : floatarray =
    [|
      Float.of_int (x + glyph_info.horiBearingX);
      Float.of_int (y + glyph_info.rows - glyph_info.horiBearingY);
      Float.of_int (x + glyph_info.horiBearingX);
      Float.of_int (y - glyph_info.horiBearingY);
      Float.of_int (x + glyph_info.width + glyph_info.horiBearingX);
      Float.of_int (y - glyph_info.horiBearingY);
      Float.of_int (x + glyph_info.width + glyph_info.horiBearingX);
      Float.of_int (y + glyph_info.rows - glyph_info.horiBearingY);
    |]
  in
  let points_idx = ref 0 in
  while !points_idx < Float.Array.length points do
    let x = Float.Array.get points !points_idx
    and y = Float.Array.get points (!points_idx + 1) in
    let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
    Float.Array.set points !points_idx x;
    Float.Array.set points (!points_idx + 1) y;
    points_idx := !points_idx + 2
  done;
  let left, right, top, bottom = get_tex_coords ~font_info ~glyph ~glyph_info in
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
    [|
      Float.Array.get points 0;
      Float.Array.get points 1;
      0.;
      0.;
      0.;
      left;
      bottom;
      Float.Array.get points 2;
      Float.Array.get points 3;
      0.;
      0.;
      0.;
      left;
      top;
      Float.Array.get points 4;
      Float.Array.get points 5;
      0.;
      0.;
      0.;
      right;
      top;
      Float.Array.get points 6;
      Float.Array.get points 7;
      0.;
      0.;
      0.;
      right;
      bottom;
    |]
  in
  let start = text_buffer.length in
  Float.Array.iteri (fun idx v -> text_buffer.buffer.{idx + start} <- v) values;
  text_buffer.length <- start + Float.Array.length values

let draw_to_gl_buffer_text () =
  Opengl.gl_bind_buffer gl_text_buffer;
  Opengl.gl_use_program text_shader_program;
  Opengl.gl_uniform_1i ~location:sampler_text_location ~value:0;
  Opengl.gl_vertex_attrib_pointer_float_type ~location:vertex_text_location
    ~size:2 ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:0;
  Opengl.gl_vertex_attrib_pointer_float_type ~location:color_text_location
    ~size:3 ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:2;
  Opengl.gl_vertex_attrib_pointer_float_type ~location:tex_coord_text_location
    ~size:2 ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:5;
  Opengl.gl_buffer_subdata_big_array ~render_buffer:text_buffer.buffer
    ~length:text_buffer.length;
  Opengl.gl_draw_arrays_with_quads
    (text_buffer.length / _EACH_POINT_FLOAT_AMOUNT_TEXT);
  text_buffer.length <- 0

let draw_to_gl_buffer () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;
  Opengl.gl_use_program ui_program;
  Opengl.gl_vertex_attrib_pointer_float_type ~location:location_point_vertex
    ~size:2 ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;
  Opengl.gl_vertex_attrib_pointer_float_type ~location:location_color ~size:4
    ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;
  Opengl.gl_buffer_subdata_big_array ~render_buffer:ui_buffer.buffer
    ~length:ui_buffer.length;
  Opengl.gl_draw_arrays_with_quads (ui_buffer.length / _EACH_POINT_FLOAT_AMOUNT);
  ui_buffer.length <- 0

let get_vertical_text_start ~(box : Ui.box) ~(font_info : Freetype.font_info) =
  try
    let bbox = Option.get box.bbox in
    let start_of_vertical =
      match box.vertical_align with
      | Some Top | None -> bbox.y
      | Some Center ->
          let start =
            bbox.y + (bbox.height / 2) - (font_info.font_height / 2)
          in
          start
      | Some Bottom -> bbox.y + bbox.height - font_info.font_height
    in
    start_of_vertical + font_info.ascender
  with Invalid_argument e -> failwith ("alignment requires a bbox;" ^ e)

let get_horizontal_text_start ~(box : Ui.box) ~(font_info : Freetype.font_info)
    ~(s : string) =
  let width_of_string =
    String.fold_left
      (fun acc c ->
        let glyph = font_info.glyph_info_with_char.(Char.code c - 32) in
        acc + glyph.x_advance)
      0 s
  in
  try
    let bbox = Option.get box.bbox in
    match box.horizontal_align with
    | Some Left | None -> bbox.x
    | Some Center -> bbox.x + (bbox.width / 2) - (width_of_string / 2)
    | Some Right -> bbox.x + bbox.width - width_of_string
  with Invalid_argument e -> failwith ("alignment requires a bbox;" ^ e)

let write_highlight_to_ui_buffer ~(points : (int * int) list) =
  let start = ui_buffer.length in
  List.iteri
    (fun i (x, y) ->
      let x, y = (Float.of_int x, Float.of_int y) in
      let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
      let idx = (i * 6) + start in
      ui_buffer.buffer.{idx} <- x;
      ui_buffer.buffer.{idx + 1} <- y;
      ui_buffer.buffer.{idx + 2} <- 0.;
      ui_buffer.buffer.{idx + 3} <- 0.;
      ui_buffer.buffer.{idx + 4} <- 1.;
      ui_buffer.buffer.{idx + 5} <- 0.5)
    points;
  ui_buffer.length <- start + (List.length points * _EACH_POINT_FLOAT_AMOUNT)

let draw_highlight ~(bbox : Ui.bounding_box) ~(font_info : Freetype.font_info)
    ~(r : Rope.rope) ~(highlight : int option * int option) ~scroll_y_offset
    ~scroll_x_offset ~text_wrap =
  let entire_points_of_highlight_quads = ref [] in
  match highlight with
  | Some highlight_start, Some highlight_end ->
      let fold_fn_for_draw_highlight acc c =
        let (Rope.Rope_Traversal_Info acc) = acc in
        match c with
        | '\n' ->
            Rope.Rope_Traversal_Info
              {
                x = bbox.x;
                y = acc.y + font_info.font_height;
                rope_pos = acc.rope_pos + 1;
              }
        | _ ->
            let gi = Ui.get_glyph_info_from_glyph ~glyph:c ~font_info in
            let x_advance = gi.x_advance in
            let next_y = acc.y + font_info.font_height in
            let ~new_x, ~new_y, ~wraps =
              Ui.get_text_wrap_info ~bbox ~glyph:c ~x:acc.x ~y:acc.y ~font_info
                ~text_wrap
            in
            (if acc.rope_pos >= highlight_start && acc.rope_pos < highlight_end
             then
               let points =
                 [
                   ( (if wraps then bbox.x else acc.x),
                     next_y + if wraps then font_info.font_height else 0 );
                   ( (if wraps then bbox.x else acc.x),
                     acc.y + if wraps then font_info.font_height else 0 );
                   ( (if wraps then bbox.x else acc.x) + x_advance,
                     acc.y + if wraps then font_info.font_height else 0 );
                   ( (if wraps then bbox.x else acc.x) + x_advance,
                     next_y + if wraps then font_info.font_height else 0 );
                 ]
               in
               entire_points_of_highlight_quads :=
                 List.append points !entire_points_of_highlight_quads);
            Rope_Traversal_Info
              { x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
      in
      ignore
        (Rope.traverse_rope ~rope:r ~handle_result:fold_fn_for_draw_highlight
           ~result:
             (Rope.Rope_Traversal_Info
                {
                  x = bbox.x + scroll_x_offset;
                  y = bbox.y + scroll_y_offset;
                  rope_pos = 0;
                }));
      write_highlight_to_ui_buffer ~points:!entire_points_of_highlight_quads
  | _ -> ()

let write_cursor_to_ui_buffer ~x ~y ~font_height =
  let points =
    [
      (x, y + font_height);
      (x, y);
      (x + Ui.text_caret_width, y);
      (x + Ui.text_caret_width, y + font_height);
    ]
  in
  let values = Float.Array.init 24 (fun _ -> 0.) in
  List.iteri
    (fun idx (x, y) ->
      let x, y = (Float.of_int x, Float.of_int y) in
      let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
      let start = idx * 6 in
      Float.Array.set values start x;
      Float.Array.set values (start + 1) y;
      Float.Array.set values (start + 2) 0.;
      Float.Array.set values (start + 3) 0.;
      Float.Array.set values (start + 4) 0.;
      Float.Array.set values (start + 5) 1.)
    points;
  Float.Array.iteri
    (fun idx v -> ui_buffer.buffer.{idx + ui_buffer.length} <- v)
    values;
  ui_buffer.length <- ui_buffer.length + Float.Array.length values

let clip_content ~(box : Ui.box) =
  Opengl.gl_enable_scissor ();
  try
    let bbox = Option.get box.bbox in
    let window_width_height = Sdl.sdl_gl_getdrawablesize () in
    let window_height_gl = window_width_height land ((1 lsl 32) - 1) in
    (* have to do some math to get the location of the bottom left corner
       because I instinctively did top left corner for box origins mainly bc
       I was adjusted to web platform but for opengl, bottom left corner is the origin *)
    Opengl.gl_scissor ~x:bbox.x
      ~y:(window_height_gl - (bbox.y + bbox.height))
      ~width:bbox.width ~height:bbox.height
  with Invalid_argument e -> failwith ("clipping needs a bbox;" ^ e)

let validate ~(box : Ui.box) =
  let rec validate' (box : Ui.box) visited =
    if List.exists (fun b -> b == box) visited then
      failwith "Recursive box structure detected"
    else
      match box.Ui.content with
      | Some (Ui.Box b) -> validate' b (box :: visited)
      | Some (Boxes list) ->
          let visited = box :: visited in
          List.iter (fun b -> validate' b visited) list
      | Some (TextAreaWithLineNumbers { container; _ }) ->
          validate' container (box :: visited)
      | Some (Text _) -> ()
      | Some (Textarea _) -> ()
      | Some (ScrollContainer { content; _ }) ->
          validate' content (box :: visited)
      | None -> ()
  in
  validate' box []

let add_event_handlers ~(box : Ui.box) =
  let rec add_event_handlers' (box : Ui.box) =
    (match box.on_event with
    | Some oc -> Ui_events.add_event_handler ~box:(Some box) ~event_handler:oc
    | None -> Ui_events.remove_event_handler ~box);
    match box.content with
    | Some (Ui.Box b) -> add_event_handlers' b
    | Some (Boxes list) -> List.iter (fun b -> add_event_handlers' b) list
    | Some (ScrollContainer { container; _ }) -> add_event_handlers' container
    | Some (TextAreaWithLineNumbers { container; _ }) ->
        add_event_handlers' container
    | Some (Text _) | Some (Textarea _) | None -> ()
  in
  add_event_handlers' box

let handle_list_of_boxes_initial_position ~(box : Ui.box) ~(d : Ui.direction)
    ~(list : Ui.box list) =
  let box_bbox = Option.value box.bbox ~default:Ui.default_bbox in
  let acc_width, acc_height =
    List.fold_left
      (fun (acc_w, acc_h) b ->
        let bbox = Option.value b.Ui.bbox ~default:Ui.default_bbox in
        match d with
        | Horizontal -> (acc_w + bbox.width, bbox.height)
        | Vertical -> (bbox.width, acc_h + bbox.height))
      (0, 0) list
  in
  let box_bbox_used_width, box_bbox_used_height =
    (box_bbox.width, box_bbox.height)
  in
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
  (x_pos + box.scroll_x_offset, y_pos + box.scroll_y_offset)

let align_inner_box_vertically ~(box : Ui.box) ~(inner_box : Ui.box) =
  match box.bbox with
  | Some bbox -> (
      let inner_box_bbox =
        Option.value inner_box.bbox ~default:Ui.default_bbox
      in
      let box_used_height = bbox.height in
      let relative_y =
        match inner_box.position_type with Relative { y; _ } -> y | _ -> 0
      in
      match box.vertical_align with
      | Some Top ->
          let y_pos = bbox.y + relative_y in
          inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
      | Some Center ->
          let y_pos =
            bbox.y + (box_used_height / 2)
            - (inner_box_bbox.height / 2)
            + relative_y
          in
          inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
      | Some Bottom ->
          let y_pos =
            bbox.y + box_used_height - inner_box_bbox.height + relative_y
          in
          inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
      | None -> ())
  | None -> ()

let align_inner_box_horizontally ~(box : Ui.box) ~(inner_box : Ui.box) =
  match box.bbox with
  | Some bbox -> (
      let inner_box_bbox =
        Option.value inner_box.bbox ~default:Ui.default_bbox
      in
      let box_used_width = bbox.width in
      let relative_x =
        match inner_box.position_type with Relative { x; _ } -> x | _ -> 0
      in
      match box.horizontal_align with
      | Some Left ->
          let x_pos = bbox.x + relative_x in
          inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
      | Some Center ->
          let x_pos =
            bbox.x + (box_used_width / 2) - (inner_box_bbox.width / 2)
            + relative_x
          in
          inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
      | Some Right ->
          let x_pos =
            bbox.x + box_used_width - inner_box_bbox.width + relative_x
          in
          inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
      | None -> ())
  | None -> ()

let draw_cursor ~(font_info : Freetype.font_info) ~(bbox : Ui.bounding_box)
    ~(r : Rope.rope) ~cursor_pos ~scroll_y_offset ~scroll_x_offset ~text_wrap =
  let start_x = bbox.x + scroll_x_offset in
  match cursor_pos with
  | Some cursor_pos ->
      let fold_fn_draw_cursor acc c =
        let (Rope.Rope_Traversal_Info acc) = acc in
        let y_pos = acc.y in
        if
          acc.rope_pos = cursor_pos && y_pos >= bbox.y
          && y_pos <= bbox.y + bbox.height
          && acc.x >= bbox.x
          && acc.x <= bbox.x + bbox.width
        then
          write_cursor_to_ui_buffer ~x:acc.x ~y:acc.y
            ~font_height:font_info.font_height;
        match c with
        | '\n' ->
            Rope.Rope_Traversal_Info
              {
                x = start_x;
                y = acc.y + font_info.font_height;
                rope_pos = acc.rope_pos + 1;
              }
        | _ ->
            let gi = Ui.get_glyph_info_from_glyph ~glyph:c ~font_info in
            let ~new_x, ~new_y, ~wraps =
              Ui.get_text_wrap_info ~bbox ~glyph:c ~x:acc.x ~y:acc.y ~font_info
                ~text_wrap
            in
            Rope_Traversal_Info
              {
                x = (if wraps then start_x + gi.x_advance else new_x);
                y = new_y;
                rope_pos = acc.rope_pos + 1;
              }
      in
      let res =
        Rope.traverse_rope ~rope:r ~handle_result:fold_fn_draw_cursor
          ~result:
            (Rope.Rope_Traversal_Info
               { x = start_x; y = bbox.y + scroll_y_offset; rope_pos = 0 })
      in
      if res.rope_pos = cursor_pos then
        write_cursor_to_ui_buffer ~x:res.x ~y:res.y
          ~font_height:font_info.font_height
  | None -> ()

let draw_text ~(s : string) ~(box : Ui.box) =
  let ~font_info, ~gl_texture_id =
    Ui.TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
  in
  Opengl.gl_bind_texture ~texture_id:gl_texture_id;
  let start_y = get_vertical_text_start ~box ~font_info + box.scroll_y_offset in
  let start_x =
    get_horizontal_text_start ~box ~font_info ~s + box.scroll_x_offset
  in
  let horizontal_pos = ref start_x in
  String.iter
    (fun c ->
      let glyph = font_info.glyph_info_with_char.(Char.code c - 32) in
      write_to_text_buffer ~glyph_info:glyph ~x:!horizontal_pos ~y:start_y
        ~glyph:c ~font_info;
      horizontal_pos := !horizontal_pos + glyph.x_advance)
    s

let draw_text_textarea ~(font_info : Freetype.font_info)
    ~(bbox : Ui.bounding_box) ~(rope : Rope.rope) ~scroll_y_offset
    ~scroll_x_offset ~text_wrap =
  let start_x = bbox.x + scroll_x_offset in
  let fold_fn_for_drawing_text acc c =
    let (Rope.Rope_Traversal_Info acc) = acc in
    if c = '\n' then
      Rope.Rope_Traversal_Info
        {
          x = start_x;
          y = acc.y + font_info.font_height;
          rope_pos = acc.rope_pos + 1;
        }
    else
      let gi = Ui.get_glyph_info_from_glyph ~glyph:c ~font_info in
      let ~new_x, ~new_y, ~wraps =
        Ui.get_text_wrap_info ~bbox ~glyph:c ~x:acc.x ~y:acc.y ~font_info
          ~text_wrap
      in
      (* descender is a negative value *)
      let descender = font_info.descender in
      let y_pos_start =
        acc.y + descender + if wraps then font_info.font_height else 0
      in
      if
        y_pos_start <= bbox.y + bbox.height
        && y_pos_start >= bbox.y
        && Bytes.length gi.bytes > 0
      then
        write_to_text_buffer
          ~x:(if wraps then start_x else acc.x)
          ~y:y_pos_start ~glyph_info:gi ~glyph:c ~font_info;
      Rope.Rope_Traversal_Info
        { rope_pos = acc.rope_pos + 1; x = new_x; y = new_y }
  in
  ignore
    (Rope.traverse_rope ~rope ~handle_result:fold_fn_for_drawing_text
       ~result:
         (Rope.Rope_Traversal_Info
            {
              rope_pos = 0;
              x = start_x;
              y = bbox.y + font_info.font_height + scroll_y_offset;
            }))

(* At first, it seems like there could be a write_rope_to_text_buffer function, BUT
  there are specific details like wrapping that I'd like to handle. Maybe there could be
  an abstraction for that specific wrapping behavior, but let's consider that later.
*)
let draw_textarea ~(font_info : Freetype.font_info) ~rope ~highlight ~cursor_pos
    ~(box : Ui.box) =
  let bbox = Option.value box.bbox ~default:Ui.default_bbox in
  match rope with
  | Some r -> (
      draw_text_textarea ~font_info ~rope:r ~bbox
        ~scroll_y_offset:box.scroll_y_offset
        ~scroll_x_offset:box.scroll_x_offset ~text_wrap:box.text_wrap;
      match !Ui.focused_element with
      | Some b when b == box ->
          draw_highlight ~r ~scroll_y_offset:box.scroll_y_offset
            ~scroll_x_offset:box.scroll_x_offset ~highlight ~bbox ~font_info
            ~text_wrap:box.text_wrap;
          draw_cursor ~r ~cursor_pos ~scroll_y_offset:box.scroll_y_offset
            ~scroll_x_offset:box.scroll_x_offset ~font_info ~bbox
            ~text_wrap:box.text_wrap
      | _ -> ())
  | None -> ()

type draw_context = { batch_writes : bool }

let rec draw_box ~(box : Ui.box) ~(context : draw_context) =
  if box.clip_content then clip_content ~box;
  (match box.bbox with
  | Some _ ->
      let Ui.{ left; top; bottom; right } = Ui.get_box_sides ~box in
      let window_width_height = Sdl.sdl_gl_getdrawablesize () in
      let window_width_gl, window_height_gl =
        (window_width_height lsr 32, window_width_height land ((1 lsl 32) - 1))
      in
      if
        left <= window_width_gl && right >= 0 && top <= window_height_gl
        && bottom >= 0
      then (
        write_container_values_to_ui_buffer ~box;
        match box.content with
        | Some (Box b) ->
            draw_box ~box:b ~context:{ batch_writes = box.batch_writes }
        | Some (Boxes list) ->
            List.iter
              (fun b ->
                draw_box ~box:b ~context:{ batch_writes = box.batch_writes })
              list
        | Some (Text { string; _ }) -> draw_text ~s:string ~box
        | Some (Textarea { text; cursor_pos; highlight_pos; _ }) ->
            let ~font_info, ~gl_texture_id =
              Ui.TextTextureInfo.get_or_add_font_size_text_texture
                ~font_size:
                  (Option.value box.font_size ~default:Freetype.font_size)
            in
            Opengl.gl_bind_texture ~texture_id:gl_texture_id;
            draw_textarea ~rope:text ~cursor_pos ~highlight:highlight_pos
              ~font_info ~box
        | Some (ScrollContainer { container; _ }) ->
            draw_box ~box:container ~context:{ batch_writes = box.batch_writes }
        | Some (TextAreaWithLineNumbers { container; _ }) ->
            draw_box ~box:container ~context:{ batch_writes = box.batch_writes }
        | None -> ())
  | None -> ());
  if (not context.batch_writes) || box.batch_writes then (
    draw_to_gl_buffer ();
    draw_to_gl_buffer_text ());
  if box.clip_content then Opengl.gl_disable_scissor ()

let handle_if_content_overflows_or_not ~(box : Ui.box)
    ~(context : Ui.ui_traversal_context) =
  match box.content with
  | Some
      (ScrollContainer
         { content; container; orientation; other_scrollcontainer; _ }) ->
      (* this case is for wrapped scrollcontainers. i dont like having the ui_traversal_context but it's the only way to
know if the current box is in a scroll container due to there not being a parent reference.
I need the context to not infinitely wrap scrollcontainers and know when to stop *)
      let Ui.
            {
              left = content_left;
              right = content_right;
              top = content_top;
              bottom = content_bottom;
            } =
        Ui.calculate_content_boundaries ~box:content
      in
      let Ui.{ width; height; _ } =
        Option.value container.bbox ~default:Ui.default_bbox
      in
      if not context.in_scrollcontainer then
        if
          content_right - content_left > width
          && box.allow_horizontal_scroll && orientation = Vertical
          && other_scrollcontainer = None
          && Option.is_some context.parent
        then (
          let parent = Option.get context.parent in
          Ui_scrollcontainers.wrap_box_contents_in_scrollcontainer ~parent ~box
            ~orientation:Horizontal;
          if
            content_bottom - content_top > height
            && box.allow_vertical_scroll && orientation = Horizontal
            && other_scrollcontainer = None
            && Option.is_some context.parent
          then
            let parent = Option.get context.parent in
            Ui_scrollcontainers.wrap_box_contents_in_scrollcontainer ~parent
              ~box ~orientation:Vertical)
  | _ ->
      let Ui.
            {
              left = content_left;
              right = content_right;
              top = content_top;
              bottom = content_bottom;
            } =
        Ui.calculate_content_boundaries ~box
      in
      let Ui.{ width; height; _ } =
        Option.value box.bbox ~default:Ui.default_bbox
      in
      if not context.in_scrollcontainer then (
        (if
           content_right - content_left > width
           && box.allow_horizontal_scroll
           && Option.is_some context.parent
         then
           let parent = Option.get context.parent in
           Ui_scrollcontainers.wrap_box_contents_in_scrollcontainer ~parent ~box
             ~orientation:Horizontal);
        if
          content_bottom - content_top > height
          && box.allow_vertical_scroll
          && Option.is_some context.parent
        then
          let parent = Option.get context.parent in
          Ui_scrollcontainers.wrap_box_contents_in_scrollcontainer ~parent ~box
            ~orientation:Vertical)

let rec calculate_ui ~(box : Ui.box) ~context =
  Ui.constrain_width_height ~box;
  handle_if_content_overflows_or_not ~box ~context;
  match box.content with
  | Some (Box b) ->
      align_inner_box_horizontally ~box ~inner_box:b;
      align_inner_box_vertically ~box ~inner_box:b;
      calculate_ui ~box:b ~context:{ context with parent = Some box }
  | Some (Boxes list) ->
      (match box.flow with
      | Some ((Horizontal | Vertical) as d) ->
          let parent_bbox =
            Option.get context.parent |> fun p -> Option.get p.bbox
          in
          Option.iter
            (fun bbox ->
              box.bbox <-
                Some { bbox with x = parent_bbox.x; y = parent_bbox.y })
            box.bbox;
          let boxes_pos =
            ref (handle_list_of_boxes_initial_position ~d ~box ~list)
          in
          List.iter
            (fun b ->
              match b.Ui.bbox with
              | Some bbbox -> begin
                  bbbox.x <- fst !boxes_pos;
                  bbbox.y <- snd !boxes_pos;
                  let bbbox_used_width, bbbox_used_height =
                    (bbbox.width, bbbox.height)
                  in
                  if Sys.getenv_opt "DEBUG" |> Option.is_some then (
                    if bbbox_used_width = 0 && d = Horizontal then
                      Printf.eprintf "flow is horizontal but width is 0\n";
                    if bbbox_used_height = 0 && d = Vertical then
                      Printf.eprintf "flow is vertical but height is 0\n");
                  boxes_pos :=
                    let x, y = !boxes_pos in
                    match d with
                    | Horizontal -> (x + bbbox_used_width, y)
                    | Vertical -> (x, y + bbbox_used_height)
                end
              | None -> ())
            list
      | None -> ());
      List.iter
        (fun b ->
          calculate_ui ~box:b ~context:{ context with parent = Some box })
        list
  | Some (Text _) -> ()
  | Some (Textarea _) -> ()
  | Some (ScrollContainer { content; scroll; container; orientation; _ }) ->
      Ui_scrollcontainers.change_content_scroll_offsets_based_off_scrollbar
        ~content ~scroll ~orientation;
      Ui_scrollcontainers.adjust_scrollbar_according_to_content_size ~content
        ~scroll ~orientation;
      (match orientation with
      | Vertical ->
          let bbox = Option.get scroll.bbox in
          if bbox.height = 0 then
            Ui_scrollcontainers.unwrap_scrollcontainer ~box
              ~unwrap_orientation:orientation
      | Horizontal ->
          let bbox = Option.get scroll.bbox in
          if bbox.width = 0 then
            Ui_scrollcontainers.unwrap_scrollcontainer ~box
              ~unwrap_orientation:orientation);
      calculate_ui ~box:container
        ~context:{ in_scrollcontainer = true; parent = Some box }
  | Some
      (TextAreaWithLineNumbers { container; line_numbers; textarea } as
       textarea_with_line_numbers) ->
      Ui_textarea_with_line_numbers.adjust_textarea_with_line_numbers
        ~textarea_with_line_numbers;
      calculate_ui ~box:container ~context:{ context with parent = Some box }
  | None -> ()

let draw ~(box : Ui.box) =
  Opengl.gl_clear_color 1. 1. 1. 1.;
  Opengl.gl_clear ();
  validate ~box;
  add_event_handlers ~box;
  calculate_ui ~box ~context:{ in_scrollcontainer = false; parent = None };
  draw_box ~box ~context:{ batch_writes = box.batch_writes };
  Sdl.sdl_gl_swapwindow Sdl.w
