open Sdl
open Freetype

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
  Render.compile_shaders_and_return_program ~vertex_id ~fragment_id
    ~vertex_src:Render.generic_vertex_shader
    ~fragment_src:Render.generic_fragment_shader

let text_shader_program =
  Render.compile_shaders_and_return_program ~vertex_id:text_vtx_id
    ~fragment_id:text_fragment_id ~vertex_src:Render.text_vertex_shader
    ~fragment_src:Render.text_fragment_shader

let gl_buffer_glyph_texture_atlas = Opengl.gl_gen_texture ()

let () =
  Opengl.gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
  Opengl.set_gl_tex_parameters_ui_text ()

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

let write_container_values_to_ui_buffer ~(box : Ui.box)
    ~(buffer : Render.render_buffer_wrapper) =
  let window_width, window_height = Sdl.sdl_get_window_size Sdl.w in
  let window_width_gl, window_height_gl = Sdl.sdl_gl_getdrawablesize () in
  let width_ratio = Float.of_int (window_width_gl / window_width) in
  let height_ratio = Float.of_int (window_height_gl / window_height) in
  let Ui.{ width; height; x; y } =
    try Option.get box.bbox with Invalid_argument e -> failwith e
  and Ui.(r, g, b, alpha) = box.background_color in
  let points =
    [| (x, y + height); (x, y); (x + width, y); (x + width, y + height) |]
    |> Array.map (fun (x', y') ->
           (Float.of_int x' *. width_ratio, Float.of_int y' *. height_ratio))
  in
  let window_width, window_height = Sdl.sdl_gl_getdrawablesize () in
  let idx = ref 0 in
  Array.iteri
    (fun i (x, y) ->
      let x = (x /. Float.of_int window_width) -. 1. in
      let y = (-.y /. Float.of_int window_height) +. 1. in
      buffer.buffer.{!idx} <- x;
      buffer.buffer.{!idx + 1} <- y;
      buffer.buffer.{!idx + 2} <- r;
      buffer.buffer.{!idx + 3} <- g;
      buffer.buffer.{!idx + 4} <- b;
      buffer.buffer.{!idx + 5} <- alpha;
      idx := (i + 1) * 6)
    points;
  buffer.length <- 24

let () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;
  Opengl.gl_buffer_data_big_array ~render_buffer:Render.Render.ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim Render.Render.ui_buffer.buffer)

let write_to_text_buffer ~(render_buf_container : Render.render_buffer_wrapper)
    ~(glyph_info : Freetype.FreeType.glyph_info_) ~x ~y ~(glyph : char)
    ~(glyph_info_with_char : (char * FreeType.glyph_info_) Array.t)
    ~(font_texture_atlas : Ui.text_texture_atlas_info) =
  let window_width, window_height = Sdl.sdl_get_window_size Sdl.w in
  let x_scaled, y_scaled =
    ( Float.of_int x /. Float.of_int window_width,
      Float.of_int y /. Float.of_int window_height )
  in
  let width_scaled = Float.of_int glyph_info.width /. Float.of_int window_width
  and height_scaled =
    Float.of_int glyph_info.rows /. Float.of_int window_height
  in
  let left, right, top, bottom =
    Render.get_tex_coords
      ~config:
        {
          glyph_info_with_char;
          font_glyph_texture_atlas_info = font_texture_atlas;
        }
      ~glyph ~glyph_info
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
  let values =
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
  Array.iteri
    (fun idx v -> render_buf_container.buffer.{idx + start} <- v)
    values;
  render_buf_container.length <- start + Array.length values

let draw_to_gl_buffer_text () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;

  Opengl.gl_uniform_1i ~location:sampler_text_location ~value:0;

  Opengl.gl_use_program text_shader_program;

  Opengl.gl_vertex_attrib_pointer_float_type ~location:vertex_text_location
    ~size:2 ~stride:Render._EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false
    ~start_idx:0;

  Opengl.gl_vertex_attrib_pointer_float_type ~location:color_text_location
    ~size:3 ~stride:Render._EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false
    ~start_idx:2;

  Opengl.gl_vertex_attrib_pointer_float_type ~location:tex_coord_text_location
    ~size:2 ~stride:Render._EACH_POINT_FLOAT_AMOUNT_TEXT ~normalized:false
    ~start_idx:5;

  Opengl.gl_buffer_subdata_big_array
    ~render_buffer:Render.Render.ui_buffer.buffer
    ~length:Render.Render.ui_buffer.length;

  Opengl.gl_draw_arrays_with_quads
    (Render.Render.ui_buffer.length / Render._EACH_POINT_FLOAT_AMOUNT);

  Bigarray.Array1.fill Render.Render.ui_buffer.buffer 0.;

  Render.Render.ui_buffer.length <- 0

let draw_to_gl_buffer () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;

  Opengl.gl_use_program ui_program;

  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:Render.Render.location_point_vertex ~size:2
    ~stride:Render._EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:Render.Render.location_color ~size:4
    ~stride:Render._EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2;

  Opengl.gl_buffer_subdata_big_array
    ~render_buffer:Render.Render.ui_buffer.buffer
    ~length:Render.Render.ui_buffer.length;

  Opengl.gl_draw_arrays_with_quads
    (Render.Render.ui_buffer.length / Render._EACH_POINT_FLOAT_AMOUNT);

  Bigarray.Array1.fill Render.Render.ui_buffer.buffer 0.;

  Render.Render.ui_buffer.length <- 0

let font_info_with_non_overridden_font_size =
  Ui.get_new_font_info_with_font_size ~font_size:FreeType.font_pixel_size
    ~face:FreeType.face

let rec draw_box ~(box : Ui.box) =
  write_container_values_to_ui_buffer ~box ~buffer:Render.Render.ui_buffer;
  draw_to_gl_buffer ();
  match box.content with
  | Some (Box b) -> draw_box ~box:b
  | Some (Boxes list) -> (
      match box.flow with
      | Some Horizontal -> (
          match box.bbox with
          | Some box_bbox ->
              let horizontal_pos = ref box_bbox.x in
              let new_boxes =
                List.map
                  (fun b ->
                    match b.Ui.bbox with
                    | Some bbbox ->
                        let new_box =
                          {
                            b with
                            Ui.bbox =
                              Some
                                {
                                  bbbox with
                                  x = !horizontal_pos;
                                  y = bbbox.Ui.y;
                                };
                          }
                        in
                        horizontal_pos := !horizontal_pos + bbbox.Ui.width;
                        new_box
                    | None -> b)
                  list
              in
              List.iter (fun b -> draw_box ~box:b) new_boxes
          | None -> failwith "box needs bbox if horizontal auto layout")
      | Some Vertical -> (
          match box.bbox with
          | Some box_bbox ->
              let vertical_pos = ref box_bbox.y in
              let new_boxes =
                List.map
                  (fun b ->
                    match b.Ui.bbox with
                    | Some bbbox ->
                        let new_box =
                          {
                            b with
                            Ui.bbox =
                              Some { bbbox with y = !vertical_pos; x = bbbox.x };
                          }
                        in
                        vertical_pos := !vertical_pos + bbbox.height;
                        new_box
                    | None -> b)
                  list
              in
              List.iter (fun b -> draw_box ~box:b) new_boxes
          | None -> failwith "box needs bbox if horizontal auto layout")
      | Some Both -> failwith "Doesn't make any sense"
      | None -> List.iter (fun b -> draw_box ~box:b) list)
  | Some (Text s) ->
      let font_info =
        if Option.is_some box.font_size then
          Ui.get_new_font_info_with_font_size
            ~font_size:(Option.get box.font_size) ~face:FreeType.face
        else font_info_with_non_overridden_font_size
      in
      let l = String.fold_right (fun c acc -> c :: acc) s [] in
      let box_bbox =
        try Option.get box.bbox with Invalid_argument e -> failwith e
      in
      let horizontal_pos = ref box_bbox.x in
      List.iter
        (fun c ->
          let found =
            Array.find_opt
              (fun (c', _) -> c' = c)
              font_info.glyph_info_with_char
          in
          let c, glyph = Option.get found in
          write_to_text_buffer ~render_buf_container:Render.Render.ui_buffer
            ~glyph_info:glyph ~x:!horizontal_pos
            ~y:(box_bbox.y + font_info.font_height)
            ~glyph:c ~font_texture_atlas:font_info.font_texture_atlas
            ~glyph_info_with_char:font_info.glyph_info_with_char;
          Opengl.gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
          Opengl.gl_teximage_2d ~bytes:font_info.font_texture_atlas.bytes
            ~width:font_info.font_texture_atlas.width
            ~height:font_info.font_texture_atlas.height;
          draw_to_gl_buffer_text ();
          horizontal_pos := !horizontal_pos + glyph.x_advance)
        l
  | None -> ()

let draw ~(box : Ui.box) =
  Opengl.gl_clear_color 1. 1. 1. 1.;
  Opengl.gl_clear ();

  draw_box ~box;

  Sdl.sdl_gl_swapwindow Sdl.w
