open Sdl
open Freetype

let _EACH_POINT_FLOAT_AMOUNT = 6
let _EACH_POINT_FLOAT_AMOUNT_TEXT = 7

type render_buffer_wrapper = {
  buffer : Opengl.render_buffer;
  mutable length : int;
}

let get_tex_coords ~(font_info : Ui.font_info) ~(glyph : char)
    ~(glyph_info : FreeType.glyph_info_) =
  let _, starting_x =
    Array.fold_left
      (fun (found, acc) (c, gi) ->
        if c = glyph || found then (true, acc)
        else (false, acc + gi.FreeType.width))
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

let default_bbox : Ui.bounding_box = { width = 0; height = 0; x = 0; y = 0 }

let write_container_values_to_ui_buffer ~(box : Ui.box)
    ~(buffer : render_buffer_wrapper) =
  let window_width, window_height = Sdl.sdl_get_window_size Sdl.w in
  let window_width_gl, window_height_gl = Sdl.sdl_gl_getdrawablesize () in
  let width_ratio = Float.of_int (window_width_gl / window_width) in
  let height_ratio = Float.of_int (window_height_gl / window_height) in
  let Ui.{ width; height; x; y } = Option.value box.bbox ~default:default_bbox
  and Ui.(r, g, b, alpha) = box.background_color in
  let points : floatarray =
    [|
      Float.of_int x *. width_ratio;
      Float.of_int (y + height) *. height_ratio;
      Float.of_int x *. width_ratio;
      Float.of_int y *. height_ratio;
      Float.of_int (x + width) *. width_ratio;
      Float.of_int y *. height_ratio;
      Float.of_int (x + width) *. width_ratio;
      Float.of_int (y + height) *. height_ratio;
    |]
  in
  let window_width, window_height = Sdl.sdl_gl_getdrawablesize () in
  let idx = ref 0 in
  let float_array_index = ref 0 in
  while !float_array_index < Float.Array.length points do
    let x = Float.Array.get points !float_array_index
    and y = Float.Array.get points (!float_array_index + 1) in
    let x = (x /. Float.of_int window_width) -. 1. in
    let y = (-.y /. Float.of_int window_height) +. 1. in
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
    ~(glyph_info : Freetype.FreeType.glyph_info_) ~x ~y ~(glyph : char)
    ~(font_info : Ui.font_info) =
  let window_width, window_height = Sdl.sdl_get_window_size Sdl.w in
  let x_scaled, y_scaled =
    ( Float.of_int x /. Float.of_int window_width,
      Float.of_int y /. Float.of_int window_height )
  in
  let width_scaled = Float.of_int glyph_info.width /. Float.of_int window_width
  and height_scaled =
    Float.of_int glyph_info.rows /. Float.of_int window_height
  in
  let left, right, top, bottom = get_tex_coords ~font_info ~glyph ~glyph_info
  and horiBearing_Y_Scaled =
    Float.of_int glyph_info.horiBearingY /. Float.of_int window_height
  and horiBearing_X_Scaled =
    Float.of_int glyph_info.horiBearingX /. Float.of_int window_width
  in
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
      x_scaled +. horiBearing_X_Scaled -. 1.;
      -.(y_scaled +. height_scaled) +. horiBearing_Y_Scaled +. 1.;
      0.;
      0.;
      0.;
      left;
      bottom;
      x_scaled +. horiBearing_X_Scaled -. 1.;
      -.y_scaled +. horiBearing_Y_Scaled +. 1.;
      0.;
      0.;
      0.;
      left;
      top;
      x_scaled +. width_scaled +. horiBearing_X_Scaled -. 1.;
      -.y_scaled +. horiBearing_Y_Scaled +. 1.;
      0.;
      0.;
      0.;
      right;
      top;
      x_scaled +. width_scaled +. horiBearing_X_Scaled -. 1.;
      -.(y_scaled +. height_scaled) +. horiBearing_Y_Scaled +. 1.;
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

module TextTextureInfo = struct
  type texture_info = {
    gl_texture_id : int;
    font_size : int;
    font_info : Ui.font_info;
  }

  let text_textures_with_different_font_sizes : texture_info list ref = ref []

  let get_or_add_font_size_text_texture ~(font_size : int) =
    let option =
      List.find_opt
        (fun { font_size = font_size'; _ } -> font_size' = font_size)
        !text_textures_with_different_font_sizes
    in
    match option with
    | Some { font_info; gl_texture_id; _ } -> (font_info, gl_texture_id)
    | None ->
        let gl_buffer_glyph_texture_atlas = Opengl.gl_gen_texture () in
        let font_info =
          Ui.get_new_font_info_with_font_size ~font_size ~face:FreeType.face
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
        (font_info, gl_buffer_glyph_texture_atlas)
end

let get_vertical_text_start ~(box : Ui.box) ~(font_info : Ui.font_info) =
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

let get_horizontal_text_start ~(box : Ui.box) ~(font_info : Ui.font_info)
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

let draw_highlight ~(bbox : Ui.bounding_box) ~(font_info : Ui.font_info)
    ~(r : Rope.rope) ~highlight ~scroll_y_offset ~highlight_buffer =
  match highlight with
  | Some (highlight_start, highlight_end) ->
      let fold_fn_for_draw_highlight
          (acc : Ui_textarea.rope_traversal_info_ Ui_textarea.traverse_info) c =
        let (Rope_Traversal_Info acc) = acc in
        match c with
        | '\n' ->
            Ui_textarea.Rope_Traversal_Info
              { acc with y = acc.y + 1; rope_pos = acc.rope_pos + 1 }
        | _ ->
            let _, glyph_info_found =
              Array.find_opt
                (fun (c', _) -> c' = c)
                font_info.glyph_info_with_char
              |> Option.get
            in
            let x_advance = glyph_info_found.x_advance in
            let next_y = acc.y + font_info.font_height in
            let new_x, new_y =
              if acc.x + x_advance > bbox.x + bbox.width then (bbox.x, next_y)
              else (acc.x + x_advance, acc.y)
            in
            (if acc.rope_pos >= highlight_start && acc.rope_pos < highlight_end
             then
               let points =
                 [
                   (acc.x, next_y);
                   (acc.x, acc.y);
                   (acc.x + x_advance, acc.y);
                   (acc.x + x_advance, next_y);
                 ]
               in
               List.iter
                 (fun (x, y) ->
                   failwith "TODO: write highlight info into ui buffer")
                 points);
            Rope_Traversal_Info
              { acc with x = new_x; y = new_y; rope_pos = acc.rope_pos + 1 }
      in
      let _ =
        Ui_textarea.traverse_rope r fold_fn_for_draw_highlight
          (Ui_textarea.Rope_Traversal_Info
             {
               x = bbox.x;
               y = bbox.y;
               rope_pos = 0;
               line_number_placements = [];
               line_num = 0;
             })
      in
      ()
  | None -> ()

let write_cursor_to_ui_buffer ~(render_buf_container : render_buffer_wrapper) ~x
    ~y ~font_height =
  let window_width, window_height = Sdl.sdl_gl_getdrawablesize () in
  let points =
    [ (x, y + font_height); (x, y); (x + 3, y); (x + 3, y + font_height) ]
  in
  let values = Float.Array.init 24 (fun _ -> 0.) in
  List.iteri
    (fun idx (x, y) ->
      let x = Float.of_int x /. Float.of_int (window_width / 2) in
      let x = x -. 1. in
      let y = Float.of_int y /. Float.of_int (window_height / 2) in
      let y = -.y +. 1. in
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

let draw_cursor ~(font_info : Ui.font_info) ~(bbox : Ui.bounding_box)
    ~(r : Rope.rope) ~cursor_pos ~scroll_y_offset =
  match cursor_pos with
  | Some cursor_pos ->
      let fold_fn_draw_cursor
          (acc : Ui_textarea.rope_traversal_info_ Ui_textarea.traverse_info) c =
        let (Rope_Traversal_Info acc) = acc in
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
            Ui_textarea.Rope_Traversal_Info
              {
                acc with
                x = bbox.x;
                y = acc.y + font_info.font_height;
                rope_pos = acc.rope_pos + 1;
              }
        | _ ->
            let _, glyph_info =
              Array.find_opt
                (fun (c', _) -> c' = c)
                font_info.glyph_info_with_char
              |> Option.get
            in
            let x_advance = glyph_info.x_advance in
            let wraps = acc.x + x_advance > bbox.x + bbox.width in
            Rope_Traversal_Info
              {
                acc with
                x = (if wraps then bbox.x + x_advance else acc.x + x_advance);
                y = (if wraps then acc.y + font_info.font_height else acc.y);
                rope_pos = acc.rope_pos + 1;
              }
      in
      let res =
        Ui_textarea.traverse_rope r fold_fn_draw_cursor
          (Ui_textarea.Rope_Traversal_Info
             {
               line_number_placements = [];
               x = bbox.x;
               y = bbox.y + (scroll_y_offset * font_info.font_height);
               rope_pos = 0;
               line_num = 0;
             })
      in
      if res.rope_pos = cursor_pos then
        write_cursor_to_ui_buffer ~render_buf_container:ui_buffer ~x:res.x
          ~y:res.y ~font_height:font_info.font_height
  | None -> ()

let draw_text ~(s : string) ~(box : Ui.box) =
  let font_info, gl_texture_id =
    TextTextureInfo.get_or_add_font_size_text_texture
      ~font_size:(Option.value box.font_size ~default:FreeType.font_size)
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

(* I'm not sure how to handle cases where the contents are positioned outside of
   the container. Originally I thought that having elements/boxes being absolutely
   positioned would be fine but that leaves problems like child contents being outside of
   the parent container which poses the question of, what should the min width/height be?
   Perhaps, restricting this functionality when child elements are only positioned relatively *)
let rec clamp_min_width_height ~(box : Ui.box) =
  if box.height_min_content || box.width_min_content then
    match box.bbox with
    | Some bbox -> (
        match box.content with
        | Some (Box b) ->
            clamp_min_width_height ~box:b;
            let inner_bbox = Option.value b.bbox ~default:default_bbox in
            if box.height_min_content then bbox.height <- inner_bbox.height;
            if box.width_min_content then bbox.width <- inner_bbox.width
        | Some (Boxes list) ->
            List.iter (fun b -> clamp_min_width_height ~box:b) list;
            let summed_heights =
              List.fold_left
                (fun acc b ->
                  acc + (Option.value b.Ui.bbox ~default:default_bbox).height)
                0 list
            in
            let summed_widths =
              List.fold_left
                (fun acc b ->
                  acc + (Option.value b.Ui.bbox ~default:default_bbox).width)
                0 list
            in
            if box.height_min_content then bbox.height <- summed_heights;
            if box.width_min_content then bbox.width <- summed_widths
        | Some (Text s) ->
            let font_info, _ =
              TextTextureInfo.get_or_add_font_size_text_texture
                ~font_size:
                  (Option.value box.font_size ~default:FreeType.font_size)
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
                  | Some (_, g) -> acc + g.FreeType.x_advance
                  | None -> acc)
                0 s
            in
            (* TODO: need to handle height_min_content when text_wrap is true *)
            if box.width_min_content then bbox.width <- string_width
        | Some (Textarea _) -> failwith "// TODO"
        | None -> ())
    | None -> ()

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
    | Some (Ui.Text _) | Some (Ui.Textarea _) | None -> ()
  in
  added_event_handlers := true;
  add_event_handlers' box

let handle_list_of_boxes_initial_position ~(box : Ui.box) ~(d : Ui.direction)
    ~(box_bbox : Ui.bounding_box) ~(list : Ui.box list) =
  let acc_width, acc_height =
    List.fold_left
      (fun (acc_w, acc_h) b ->
        let bbox = Option.value b.Ui.bbox ~default:default_bbox in
        match d with
        | Horizontal -> (acc_w + bbox.width, bbox.height)
        | Vertical -> (bbox.width, acc_h + bbox.height))
      (0, 0) list
  in
  let x_pos =
    match box.horizontal_align with
    | Some Left | None -> box_bbox.x
    | Some Center -> box_bbox.x + (box_bbox.width / 2) - (acc_width / 2)
    | Some Right -> box_bbox.x + box_bbox.width - acc_width
  in
  let y_pos =
    match box.vertical_align with
    | Some Top | None -> box_bbox.y
    | Some Center -> box_bbox.y + (box_bbox.height / 2) - (acc_height / 2)
    | Some Bottom -> box_bbox.y + box_bbox.height - acc_height
  in
  (x_pos, y_pos)

let align_inner_box_vertically ~(box : Ui.box) ~(inner_box : Ui.box) =
  match box.bbox with
  | Some bbox -> (
      let inner_box_bbox = Option.value inner_box.bbox ~default:default_bbox in
      match box.vertical_align with
      | Some Top ->
          let y_pos =
            bbox.y
            + if inner_box.position_type = Relative then inner_box_bbox.y else 0
          in
          inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
      | Some Center ->
          let y_pos =
            bbox.y + (bbox.height / 2)
            - (inner_box_bbox.height / 2)
            + if inner_box.position_type = Relative then inner_box_bbox.y else 0
          in
          inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
      | Some Bottom ->
          let y_pos =
            bbox.y + bbox.height - inner_box_bbox.height
            + if inner_box.position_type = Relative then inner_box_bbox.y else 0
          in
          inner_box.bbox <- Some { inner_box_bbox with y = y_pos }
      | None -> (
          match inner_box.position_type with
          | Relative ->
              inner_box.bbox <-
                Some { inner_box_bbox with y = bbox.y + inner_box_bbox.y }
          | Absolute ->
              inner_box.bbox <-
                Some { inner_box_bbox with y = inner_box_bbox.y }))
  | None -> ()

let align_inner_box_horizontally ~(box : Ui.box) ~(inner_box : Ui.box) =
  match box.bbox with
  | Some bbox -> (
      let inner_box_bbox = Option.value inner_box.bbox ~default:default_bbox in
      match box.horizontal_align with
      | Some Left ->
          let x_pos =
            bbox.x
            + if inner_box.position_type = Relative then inner_box_bbox.x else 0
          in
          inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
      | Some Center ->
          let x_pos =
            bbox.x + (bbox.width / 2) - (inner_box_bbox.width / 2)
            + if inner_box.position_type = Relative then inner_box_bbox.x else 0
          in
          inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
      | Some Right ->
          let x_pos =
            bbox.x + bbox.width - inner_box_bbox.width
            + if inner_box.position_type = Relative then inner_box_bbox.x else 0
          in
          inner_box.bbox <- Some { inner_box_bbox with x = x_pos }
      | None -> (
          match inner_box.position_type with
          | Relative ->
              inner_box.bbox <-
                Some { inner_box_bbox with x = bbox.x + inner_box_bbox.x }
          | Absolute ->
              inner_box.bbox <-
                Some { inner_box_bbox with x = inner_box_bbox.x }))
  | None -> ()

let _ = Opengl.gl_enable_texture_2d ()
let _ = Opengl.gl_enable_blending ()

let draw_text_textarea ~(font_info : Ui.font_info) ~(bbox : Ui.bounding_box)
    ~(rope : Rope.rope) ~text_buffer ~window_width ~window_height
    ~scroll_y_offset =
  let fold_fn_for_drawing_text
      (acc : Ui_textarea.rope_traversal_info_ Ui_textarea.traverse_info) c =
    let (Ui_textarea.Rope_Traversal_Info acc) = acc in
    if c = '\n' then
      Ui_textarea.Rope_Traversal_Info
        {
          x = acc.x;
          y = acc.y + font_info.font_height;
          rope_pos = acc.rope_pos + 1;
          line_num = acc.line_num + 1;
          line_number_placements =
            (acc.line_num + 1, acc.y + font_info.font_height)
            :: acc.line_number_placements;
        }
    else
      let gi = ref None in
      let len = Array.length font_info.glyph_info_with_char in
      for glyph_info_index = 0 to len - 1 do
        let c', gi' = font_info.glyph_info_with_char.(glyph_info_index) in
        if c' = c then gi := Some gi'
      done;
      let gi = Option.get !gi in
      let x_advance = gi.x_advance in
      let wraps = acc.x + x_advance > bbox.x + bbox.width in
      let new_x, new_y =
        if wraps then (bbox.x + x_advance, acc.y + font_info.font_height)
        else (acc.x + x_advance, acc.y)
      in
      (* descender is a negative value *)
      let descender = font_info.descender in
      let y_pos_start =
        acc.y
        + (scroll_y_offset * font_info.font_height)
        + descender
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
      Ui_textarea.Rope_Traversal_Info
        { acc with rope_pos = acc.rope_pos + 1; x = new_x; y = new_y }
  in
  Ui_textarea.traverse_rope rope fold_fn_for_drawing_text
    (Ui_textarea.Rope_Traversal_Info
       {
         line_number_placements = [ (1, font_info.font_height) ];
         rope_pos = 0;
         x = bbox.x;
         y = bbox.y + font_info.font_height;
         line_num = 1;
       })

(* At first, it seems like there could be a write_rope_to_text_buffer function, BUT
  there are specific details like wrapping that I'd like to handle. Maybe there could be
  an abstraction for that specific wrapping behavior, but let's consider that later.
*)
let draw_textarea' ~(font_info : Ui.font_info) ~rope ~(scroll_y_offset : int)
    ~(bbox : Ui.bounding_box) ~highlight ~cursor_pos ~(box : Ui.box) =
  let window_dims = Sdl.sdl_gl_getdrawablesize () in
  let window_width, window_height = window_dims in
  match rope with
  | Some r -> (
      let Ui_textarea.{ line_number_placements; _ } =
        draw_text_textarea ~font_info ~rope:r ~window_width ~window_height ~bbox
          ~scroll_y_offset ~text_buffer:ui_buffer
      in
      draw_to_gl_buffer_text ();
      match !Ui.focused_element with
      | Some b when b == box ->
          draw_highlight ~r ~scroll_y_offset ~highlight ~bbox ~font_info
            ~highlight_buffer:ui_buffer;
          draw_to_gl_buffer ();
          draw_cursor ~r ~cursor_pos ~scroll_y_offset ~font_info ~bbox;
          draw_to_gl_buffer ()
      | None | _ -> ())
  | None -> ()

let () =
  Opengl.gl_enable_vertex_attrib_array vertex_text_location;
  Opengl.gl_enable_vertex_attrib_array color_text_location;
  Opengl.gl_enable_vertex_attrib_array tex_coord_text_location;
  Opengl.gl_enable_vertex_attrib_array location_point_vertex;
  Opengl.gl_enable_vertex_attrib_array location_color

let draw_textarea ~(rope : Rope.rope option) ~cursor_pos ~highlight_pos
    ~scroll_y_offset ~scroll_x_offset ~font_info ~bbox ~box =
  Opengl.gl_use_program ui_program;

  Opengl.gl_vertex_attrib_pointer_float_type ~location:location_point_vertex
    ~size:2 ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

  Opengl.gl_vertex_attrib_pointer_float_type ~location:location_color ~size:4
    ~stride:_EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;

  Opengl.gl_buffer_subdata_big_array ~render_buffer:ui_buffer.buffer
    ~length:ui_buffer.length;

  Opengl.gl_draw_arrays_with_quads (ui_buffer.length / _EACH_POINT_FLOAT_AMOUNT);

  ui_buffer.length <- 0;

  draw_textarea' ~font_info ~rope ~cursor_pos ~highlight:highlight_pos ~bbox
    ~scroll_y_offset ~box

let rec draw_box ~(box : Ui.box) =
  if not !validated then validate ~box;
  if not !added_event_handlers then add_event_handlers ~box;
  if box.clip_content then clip_content ~box;
  clamp_min_width_height ~box;
  (match box.bbox with
  | Some _ ->
      let Ui.{ left; top; bottom; right } = Ui.get_box_sides ~box in
      let window_width_gl, window_height_gl = Sdl.sdl_gl_getdrawablesize () in
      if
        left >= 0 && right <= window_width_gl && top >= 0
        && bottom <= window_height_gl
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
                let box_bbox = Option.value box.bbox ~default:default_bbox in
                let boxes_pos =
                  ref
                    (handle_list_of_boxes_initial_position ~d ~box ~box_bbox
                       ~list)
                in
                List.iter
                  (fun b ->
                    match b.Ui.bbox with
                    | Some bbbox -> (
                        bbbox.x <- fst !boxes_pos;
                        bbbox.y <- snd !boxes_pos;
                        boxes_pos :=
                          let x, y = !boxes_pos in
                          match d with
                          | Horizontal -> (x + bbbox.width, y)
                          | Vertical -> (x, y + bbbox.height))
                    | None -> ())
                  list;
                List.iter (fun b -> draw_box ~box:b) list
            | None -> List.iter (fun b -> draw_box ~box:b) list)
        | Some (Text s) -> draw_text ~s ~box
        | Some
            (Textarea
               {
                 text;
                 cursor_pos;
                 highlight_pos;
                 scroll_y_offset;
                 scroll_x_offset;
               }) ->
            clip_content ~box;
            let font_info, gl_texture_id =
              TextTextureInfo.get_or_add_font_size_text_texture
                ~font_size:
                  (Option.value box.font_size ~default:FreeType.font_size)
            in
            Opengl.gl_bind_texture ~texture_id:gl_texture_id;
            draw_textarea ~rope:(Some text) ~cursor_pos ~highlight_pos
              ~font_info
              ~bbox:(Option.value box.bbox ~default:default_bbox)
              ~scroll_y_offset ~scroll_x_offset ~box;
            Opengl.gl_disable_scissor ()
        | None -> ())
  | None -> ());
  if box.clip_content then Opengl.gl_disable_scissor ()

let draw ~(box : Ui.box) =
  Opengl.gl_clear_color 1. 1. 1. 1.;
  Opengl.gl_clear ();

  draw_box ~box;

  Sdl.sdl_gl_swapwindow Sdl.w
