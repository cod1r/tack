open Sdl
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
  let _, starting_x =
    Array.fold_left
      (fun (found, acc) ((c, gi) : char * Freetype.glyph_info_) ->
        if c = glyph || found then (true, acc)
        else (false, acc + gi.Freetype.width))
      (false, 0) font_info.glyph_info_with_char
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

let text_fragment_id =
  match Opengl.gl_create_fragment_shader () with
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

let text_shader_program =
  Opengl.compile_shaders_and_return_program ~vertex_id:text_vertex_id
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

let gl_ui_lib_buffer = Opengl.gl_gen_one_buffer ()

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
  let window_width, window_height = Sdl.sdl_gl_getdrawablesize () in
  ( (x *. width_ratio /. Float.of_int window_width) -. 1.,
    (-.y *. height_ratio /. Float.of_int window_height) +. 1. )

let write_container_values_to_ui_buffer ~(box : Ui.box)
    ~(buffer : render_buffer_wrapper) =
  let Ui.{ width; height; x; y; _ } =
    Option.value box.bbox ~default:Ui.default_bbox
  and Ui.(r, g, b, alpha) = box.background_color in
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
  let idx = ref 0 in
  let float_array_index = ref 0 in
  while !float_array_index < Float.Array.length points do
    let x = Float.Array.get points !float_array_index
    and y = Float.Array.get points (!float_array_index + 1) in
    let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
    buffer.buffer.{!idx} <- x;
    buffer.buffer.{!idx + 1} <- y;
    buffer.buffer.{!idx + 2} <- r;
    buffer.buffer.{!idx + 3} <- g;
    buffer.buffer.{!idx + 4} <- b;
    buffer.buffer.{!idx + 5} <- alpha;
    idx := ((!float_array_index / 2) + 1) * 6;
    float_array_index := !float_array_index + 2
  done;
  buffer.length <- 24

let () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;
  Opengl.gl_buffer_data_big_array ~render_buffer:ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim ui_buffer.buffer)

let write_text_to_ui_buffer ~(render_buf_container : render_buffer_wrapper)
    ~(glyph_info : Freetype.glyph_info_) ~x ~y ~(glyph : char)
    ~(font_info : Freetype.font_info) =
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
  let start = render_buf_container.length in
  Float.Array.iteri
    (fun idx v -> render_buf_container.buffer.{idx + start} <- v)
    values;
  render_buf_container.length <- start + Float.Array.length values

let draw_to_gl_buffer_text () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;
  Opengl.gl_use_program text_shader_program;
  Opengl.gl_uniform_1i ~location:sampler_text_location ~value:0;
  Opengl.gl_vertex_attrib_pointer_float_type ~location:vertex_text_location
    ~size:2 ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:0;
  Opengl.gl_vertex_attrib_pointer_float_type ~location:color_text_location
    ~size:3 ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:2;
  Opengl.gl_vertex_attrib_pointer_float_type ~location:tex_coord_text_location
    ~size:2 ~stride:_EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false ~start_idx:5;
  Opengl.gl_buffer_subdata_big_array ~render_buffer:ui_buffer.buffer
    ~length:ui_buffer.length;
  Opengl.gl_draw_arrays_with_quads
    (ui_buffer.length / _EACH_POINT_FLOAT_AMOUNT_TEXT);
  ui_buffer.length <- 0

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
        try
          let _, glyph =
            Array.find_opt
              (fun (c', _) -> c' = c)
              font_info.glyph_info_with_char
            |> Option.get
          in
          acc + glyph.x_advance
        with Invalid_argument e -> failwith (__FUNCTION__ ^ ";" ^ e))
      0 s
  in
  try
    let bbox = Option.get box.bbox in
    match box.horizontal_align with
    | Some Left | None -> bbox.x
    | Some Center -> bbox.x + (bbox.width / 2) - (width_of_string / 2)
    | Some Right -> bbox.x + bbox.width - width_of_string
  with Invalid_argument e -> failwith ("alignment requires a bbox;" ^ e)

let write_highlight_to_ui_buffer ~(buffer : render_buffer_wrapper)
    ~(points : (int * int) list) =
  let start = buffer.length in
  List.iteri
    (fun i (x, y) ->
      let x, y = (Float.of_int x, Float.of_int y) in
      let x, y = transform_xy_coords_to_opengl_viewport_coords ~x ~y in
      let idx = (i * 6) + start in
      buffer.buffer.{idx} <- x;
      buffer.buffer.{idx + 1} <- y;
      buffer.buffer.{idx + 2} <- 0.;
      buffer.buffer.{idx + 3} <- 0.;
      buffer.buffer.{idx + 4} <- 1.;
      buffer.buffer.{idx + 5} <- 0.5)
    points;
  buffer.length <- List.length points * 6

let draw_highlight ~(bbox : Ui.bounding_box) ~(font_info : Freetype.font_info)
    ~(r : Rope.rope) ~(highlight : int option * int option) ~scroll_y_offset =
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
                { x = bbox.x; y = bbox.y + scroll_y_offset; rope_pos = 0 }));
      write_highlight_to_ui_buffer ~buffer:ui_buffer
        ~points:!entire_points_of_highlight_quads
  | _ -> ()

let write_cursor_to_ui_buffer ~(render_buf_container : render_buffer_wrapper) ~x
    ~y ~font_height =
  let points =
    [ (x, y + font_height); (x, y); (x + 3, y); (x + 3, y + font_height) ]
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
  Float.Array.iteri (fun idx v -> render_buf_container.buffer.{idx} <- v) values;
  render_buf_container.length <- Float.Array.length values

let draw_cursor ~(font_info : Freetype.font_info) ~(bbox : Ui.bounding_box)
    ~(r : Rope.rope) ~cursor_pos ~scroll_y_offset =
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
          write_cursor_to_ui_buffer ~render_buf_container:ui_buffer ~x:acc.x
            ~y:acc.y ~font_height:font_info.font_height;
        match c with
        | '\n' ->
            Rope.Rope_Traversal_Info
              {
                x = bbox.x;
                y = acc.y + font_info.font_height;
                rope_pos = acc.rope_pos + 1;
              }
        | _ ->
            let ~new_x, ~new_y, .. =
              Ui.get_text_wrap_info ~bbox ~glyph:c ~x:acc.x ~y:acc.y ~font_info
            in
            Rope_Traversal_Info
              { x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
      in
      let res =
        Rope.traverse_rope ~rope:r ~handle_result:fold_fn_draw_cursor
          ~result:
            (Rope.Rope_Traversal_Info
               { x = bbox.x; y = bbox.y + scroll_y_offset; rope_pos = 0 })
      in
      if res.rope_pos = cursor_pos then
        write_cursor_to_ui_buffer ~render_buf_container:ui_buffer ~x:res.x
          ~y:res.y ~font_height:font_info.font_height
  | None -> ()

let draw_text ~(s : string) ~(box : Ui.box) =
  let ~font_info, ~gl_texture_id =
    Ui.TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
  in
  Opengl.gl_bind_texture ~texture_id:gl_texture_id;
  let start_y = get_vertical_text_start ~box ~font_info in
  let start_x = get_horizontal_text_start ~box ~font_info ~s in
  let horizontal_pos = ref start_x in
  String.iter
    (fun c ->
      let found =
        Array.find_opt (fun (c', _) -> c' = c) font_info.glyph_info_with_char
      in
      let c, glyph = Option.get found in
      write_text_to_ui_buffer ~render_buf_container:ui_buffer ~glyph_info:glyph
        ~x:!horizontal_pos ~y:start_y ~glyph:c ~font_info;
      horizontal_pos := !horizontal_pos + glyph.x_advance)
    s;
  draw_to_gl_buffer_text ()

let get_available_size_for_maxed_constrained_inner_boxes
    ~(fixed_sized_boxes : Ui.box list) ~(parent_bbox : Ui.bounding_box)
    ~(measurement : [< `Width | `Height ]) ~number_of_constrained =
  let summed_fixed, parent_measurement =
    match measurement with
    | `Width ->
        ( List.fold_left
            (fun acc b ->
              (Option.value b.Ui.bbox ~default:Ui.default_bbox).width + acc)
            0 fixed_sized_boxes,
          parent_bbox.width )
    | `Height ->
        ( List.fold_left
            (fun acc b ->
              (Option.value b.Ui.bbox ~default:Ui.default_bbox).height + acc)
            0 fixed_sized_boxes,
          parent_bbox.height )
  in
  let left_over = max 0 (parent_measurement - summed_fixed) in
  left_over / number_of_constrained

let handle_maximizing_of_inner_content_size ~(parent_box : Ui.box) =
  let parent_bbox = Option.value parent_box.bbox ~default:Ui.default_bbox in
  match parent_box.content with
  | Some (Box b) -> (
      (match b.width_constraint with
      | Some Max ->
          let b_bbox = Option.value b.bbox ~default:Ui.default_bbox in
          b.bbox <- Some { b_bbox with width = parent_bbox.width }
      | Some Min | None -> ());
      match b.height_constraint with
      | Some Max ->
          let b_bbox = Option.value b.bbox ~default:Ui.default_bbox in
          b.bbox <- Some { b_bbox with height = parent_bbox.height }
      | Some Min | None -> ())
  | Some (Boxes list) -> (
      let fixed_width_boxes =
        List.filter (fun b -> Option.is_none b.Ui.width_constraint) list
      in
      let fixed_height_boxes =
        List.filter (fun b -> Option.is_none b.Ui.height_constraint) list
      in
      let constrained_width_boxes =
        List.filter (fun b -> Option.is_some b.Ui.width_constraint) list
      in
      let constrained_height_boxes =
        List.filter (fun b -> Option.is_some b.Ui.height_constraint) list
      in
      match parent_box.flow with
      | Some Horizontal ->
          (if List.length constrained_width_boxes > 0 then
             let width_for_each_constrained_box =
               get_available_size_for_maxed_constrained_inner_boxes
                 ~fixed_sized_boxes:fixed_width_boxes ~parent_bbox
                 ~measurement:`Width
                 ~number_of_constrained:(List.length constrained_width_boxes)
             in
             List.iter
               (fun b ->
                 let bbox = Option.value b.Ui.bbox ~default:Ui.default_bbox in
                 b.bbox <-
                   Some { bbox with width = width_for_each_constrained_box })
               constrained_width_boxes);
          List.iter
            (fun b ->
              let bbox = Option.value b.Ui.bbox ~default:Ui.default_bbox in
              b.bbox <- Some { bbox with height = parent_bbox.height })
            constrained_height_boxes
      | Some Vertical ->
          (if List.length constrained_height_boxes > 0 then
             let height_for_each_constrained_box =
               get_available_size_for_maxed_constrained_inner_boxes
                 ~fixed_sized_boxes:fixed_height_boxes ~parent_bbox
                 ~measurement:`Height
                 ~number_of_constrained:(List.length constrained_height_boxes)
             in
             List.iter
               (fun b ->
                 let bbox = Option.value b.Ui.bbox ~default:Ui.default_bbox in
                 b.bbox <-
                   Some { bbox with height = height_for_each_constrained_box })
               constrained_height_boxes);
          List.iter
            (fun b ->
              let bbox = Option.value b.Ui.bbox ~default:Ui.default_bbox in
              b.bbox <- Some { bbox with width = parent_bbox.width })
            constrained_width_boxes
      | _ -> ())
  | Some (Text _) -> ()
  | Some (Textarea _) -> ()
  | Some (ScrollContainer _) -> ()
  | None -> ()

let rec clamp_width_or_height_to_content_size ~(box : Ui.box)
    ~(measurement : [< `Width | `Height ]) =
  let bbox = Option.value box.bbox ~default:Ui.default_bbox in
  match box.content with
  | Some (Box b) -> (
      constrain_width_height ~box:b;
      let inner_bbox = Option.value b.bbox ~default:Ui.default_bbox in
      match measurement with
      | `Width -> box.bbox <- Some { bbox with width = inner_bbox.width }
      | `Height -> box.bbox <- Some { bbox with height = inner_bbox.height })
  | Some (Boxes list) -> (
      List.iter (fun b -> constrain_width_height ~box:b) list;
      match box.flow with
      | Some Vertical -> (
          let summed_size =
            List.fold_left
              (fun acc b ->
                acc + (Option.value b.Ui.bbox ~default:Ui.default_bbox).height)
              0 list
          in
          let max_width =
            List.fold_left
              (fun acc b ->
                max acc (Option.value b.Ui.bbox ~default:Ui.default_bbox).width)
              0 list
          in
          match measurement with
          | `Width -> box.bbox <- Some { bbox with width = max_width }
          | `Height -> box.bbox <- Some { bbox with height = summed_size })
      | Some Horizontal -> (
          let summed_size =
            List.fold_left
              (fun acc b ->
                acc + (Option.value b.Ui.bbox ~default:Ui.default_bbox).width)
              0 list
          in
          let max_height =
            List.fold_left
              (fun acc b ->
                max acc (Option.value b.Ui.bbox ~default:Ui.default_bbox).height)
              0 list
          in
          match measurement with
          | `Width -> box.bbox <- Some { bbox with width = summed_size }
          | `Height -> box.bbox <- Some { bbox with height = max_height })
      | None -> ())
  | Some (Text s) -> (
      let ~font_info, .. =
        Ui.TextTextureInfo.get_or_add_font_size_text_texture
          ~font_size:(Option.value box.font_size ~default:Freetype.font_size)
      in
      let string_width =
        String.fold_left
          (fun acc c ->
            let op =
              Array.find_opt
                (fun (c', _) -> c' = c)
                font_info.glyph_info_with_char
            in
            match op with
            | Some (_, g) -> acc + g.Freetype.x_advance
            | None -> acc)
          0 s
      in
      (* TODO: need to handle height when text_wrap is true *)
      match measurement with
      | `Width -> box.bbox <- Some { bbox with width = string_width }
      | `Height -> ())
  | Some (Textarea _) -> failwith "// TODO"
  | Some (ScrollContainer { content; scroll; _ }) -> (
      match measurement with
      | `Width ->
          let { width = content_width; _ } : Ui.bounding_box =
            Option.value content.bbox ~default:Ui.default_bbox
          and { width = scroll_width; _ } : Ui.bounding_box =
            Option.value scroll.bbox ~default:Ui.default_bbox
          in
          box.bbox <- Some { bbox with width = content_width + scroll_width }
      | `Height ->
          let { height = content_height; _ } : Ui.bounding_box =
            Option.value content.bbox ~default:Ui.default_bbox
          and { height = scroll_height; _ } : Ui.bounding_box =
            Option.value scroll.bbox ~default:Ui.default_bbox
          in
          box.bbox <- Some { bbox with height = content_height + scroll_height }
      )
  | None -> ()

(* I'm not sure how to handle cases where the contents are positioned outside of
   the container. Originally I thought that having elements/boxes being absolutely
   positioned would be fine but that leaves problems like child contents being outside of
   the parent container which poses the question of, what should the min width/height be?
   Perhaps, restricting this functionality when child elements are only positioned relatively *)
and constrain_width_height ~(box : Ui.box) =
  (match box.width_constraint with
  | Some Min -> clamp_width_or_height_to_content_size ~box ~measurement:`Width
  | Some Max | None -> ());
  (match box.height_constraint with
  | Some Min -> clamp_width_or_height_to_content_size ~box ~measurement:`Height
  | Some Max | None -> ());
  handle_maximizing_of_inner_content_size ~parent_box:box

let clip_content ~(box : Ui.box) =
  Opengl.gl_enable_scissor ();
  try
    let bbox = Option.get box.bbox in
    let _, window_height_gl = Sdl.sdl_gl_getdrawablesize () in
    (* have to do some math to get the location of the bottom left corner
       because I instinctively did top left corner for box origins mainly bc
       I was adjusted to web platform but for opengl, bottom left corner is the origin *)
    Opengl.gl_scissor ~x:bbox.x
      ~y:(window_height_gl - (bbox.y + bbox.height))
      ~width:bbox.width ~height:bbox.height
  with Invalid_argument e -> failwith ("clipping needs a bbox;" ^ e)

let validated = ref false

let validate ~(box : Ui.box) =
  let rec validate' box visited =
    if List.exists (fun b -> b == box) visited then
      failwith "Recursive box structure detected"
    else
      match box.Ui.content with
      | Some (Ui.Box b) -> validate' b (box :: visited)
      | Some (Ui.Boxes list) ->
          let visited = box :: visited in
          List.iter (fun b -> validate' b visited) list
      | Some (Ui.Text _) -> ()
      | Some (Ui.Textarea _) -> ()
      | Some (Ui.ScrollContainer { content; _ }) ->
          validate' content (box :: visited)
      | None -> ()
  in
  validated := true;
  validate' box []

let added_event_handlers = ref false

let add_event_handlers ~(box : Ui.box) =
  let rec add_event_handlers' (box : Ui.box) =
    (match box.on_event with
    | Some oc -> Ui_events.add_event_handler ~box:(Some box) ~event_handler:oc
    | None -> ());
    match box.content with
    | Some (Ui.Box b) -> add_event_handlers' b
    | Some (Ui.Boxes list) -> List.iter (fun b -> add_event_handlers' b) list
    | Some (Ui.ScrollContainer { container; _ }) ->
        add_event_handlers' container
    | Some (Ui.Text _) | Some (Ui.Textarea _) | None -> ()
  in
  added_event_handlers := true;
  add_event_handlers' box

let handle_list_of_boxes_initial_position ~(box : Ui.box) ~(d : Ui.direction)
    ~(list : Ui.box list) =
  let box_bbox = Option.value box.bbox ~default:Ui.default_bbox in
  let acc_width, acc_height =
    List.fold_left
      (fun (acc_w, acc_h) b ->
        let bbox = Option.value b.Ui.bbox ~default:Ui.default_bbox in
        let bbox_used_width, bbox_used_height = (bbox.width, bbox.height) in
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
  (x_pos, y_pos)

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
      | None -> (
          match inner_box.position_type with
          | Relative { x; _ } ->
              inner_box.bbox <- Some { inner_box_bbox with x = bbox.x + x }
          | Absolute ->
              inner_box.bbox <-
                Some { inner_box_bbox with x = inner_box_bbox.x }))
  | None -> ()

let draw_text_textarea ~(font_info : Freetype.font_info)
    ~(bbox : Ui.bounding_box) ~(rope : Rope.rope) ~text_buffer ~scroll_y_offset
    =
  let fold_fn_for_drawing_text acc c =
    let (Rope.Rope_Traversal_Info acc) = acc in
    if c = '\n' then
      Rope.Rope_Traversal_Info
        {
          x = bbox.x;
          y = acc.y + font_info.font_height;
          rope_pos = acc.rope_pos + 1;
        }
    else
      let gi = Ui.get_glyph_info_from_glyph ~glyph:c ~font_info in
      let x_advance = gi.x_advance in
      let ~new_x, ~new_y, ~wraps =
        Ui.get_text_wrap_info ~bbox ~glyph:c ~x:acc.x ~y:acc.y ~font_info
      in
      (* descender is a negative value *)
      let descender = font_info.descender in
      let y_pos_start =
        acc.y + scroll_y_offset + descender
        + if wraps then font_info.font_height else 0
      in
      if
        y_pos_start <= bbox.y + bbox.height
        && y_pos_start >= bbox.y
        && Bytes.length gi.bytes > 0
      then
        write_text_to_ui_buffer ~render_buf_container:text_buffer
          ~x:(if wraps then bbox.x else acc.x)
          ~y:y_pos_start ~glyph_info:gi ~glyph:c ~font_info;
      Rope.Rope_Traversal_Info
        { rope_pos = acc.rope_pos + 1; x = new_x; y = new_y }
  in
  ignore
    (Rope.traverse_rope ~rope ~handle_result:fold_fn_for_drawing_text
       ~result:
         (Rope.Rope_Traversal_Info
            { rope_pos = 0; x = bbox.x; y = bbox.y + font_info.font_height }))

(* At first, it seems like there could be a write_rope_to_text_buffer function, BUT
  there are specific details like wrapping that I'd like to handle. Maybe there could be
  an abstraction for that specific wrapping behavior, but let's consider that later.
*)
let draw_textarea ~(font_info : Freetype.font_info) ~rope
    ~(scroll_y_offset : int) ~highlight ~cursor_pos ~(box : Ui.box) =
  let bbox = Option.value box.bbox ~default:Ui.default_bbox in
  match rope with
  | Some r -> (
      draw_text_textarea ~font_info ~rope:r ~bbox ~scroll_y_offset
        ~text_buffer:ui_buffer;
      draw_to_gl_buffer_text ();
      match !Ui.focused_element with
      | Some b when b == box ->
          draw_highlight ~r ~scroll_y_offset ~highlight ~bbox ~font_info;
          draw_to_gl_buffer ();
          draw_cursor ~r ~cursor_pos ~scroll_y_offset ~font_info ~bbox;
          draw_to_gl_buffer ()
      | None | _ -> ())
  | None -> ()

let rec draw_box ~(box : Ui.box) =
  if not !validated then validate ~box;
  if not !added_event_handlers then add_event_handlers ~box;
  if box.clip_content then clip_content ~box;
  constrain_width_height ~box;
  (match box.bbox with
  | Some _ ->
      let Ui.{ left; top; bottom; right } = Ui.get_box_sides ~box in
      let window_width_gl, window_height_gl = Sdl.sdl_gl_getdrawablesize () in
      if
        left <= window_width_gl && right >= 0 && top <= window_height_gl
        && bottom >= 0
      then (
        write_container_values_to_ui_buffer ~box ~buffer:ui_buffer;
        draw_to_gl_buffer ();
        match box.content with
        | Some (Box b) ->
            align_inner_box_horizontally ~box ~inner_box:b;
            align_inner_box_vertically ~box ~inner_box:b;
            draw_box ~box:b
        | Some (Boxes list) -> (
            match box.flow with
            | Some ((Horizontal | Vertical) as d) ->
                let boxes_pos =
                  ref (handle_list_of_boxes_initial_position ~d ~box ~list)
                in
                List.iter
                  (fun b ->
                    match b.Ui.bbox with
                    | Some bbbox -> (
                        bbbox.x <- fst !boxes_pos;
                        bbbox.y <- snd !boxes_pos;
                        let bbbox_used_width, bbbox_used_height =
                          (bbbox.width, bbbox.height)
                        in
                        boxes_pos :=
                          let x, y = !boxes_pos in
                          match d with
                          | Horizontal -> (x + bbbox_used_width, y)
                          | Vertical -> (x, y + bbbox_used_height))
                    | None -> ())
                  list;
                List.iter (fun b -> draw_box ~box:b) list
            | None -> List.iter (fun b -> draw_box ~box:b) list)
        | Some (Text s) -> draw_text ~s ~box
        | Some (Textarea { text; cursor_pos; highlight_pos; _ }) ->
            clip_content ~box;
            let ~font_info, ~gl_texture_id =
              Ui.TextTextureInfo.get_or_add_font_size_text_texture
                ~font_size:
                  (Option.value box.font_size ~default:Freetype.font_size)
            in
            Opengl.gl_bind_texture ~texture_id:gl_texture_id;
            draw_textarea ~rope:text ~cursor_pos ~highlight:highlight_pos
              ~font_info ~scroll_y_offset:box.scroll_y_offset ~box;
            Opengl.gl_disable_scissor ()
        | Some (ScrollContainer { content; scroll; container }) ->
            draw_box ~box:container
        | None -> ())
  | None -> ());
  if box.clip_content then Opengl.gl_disable_scissor ()

let draw ~(box : Ui.box) =
  Opengl.gl_clear_color 1. 1. 1. 1.;
  Opengl.gl_clear ();
  draw_box ~box;
  Sdl.sdl_gl_swapwindow Sdl.w
