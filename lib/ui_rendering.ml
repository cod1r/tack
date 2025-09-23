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
  let ui_info = Ui.get_ui_information () in
  Opengl.gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
  Opengl.set_gl_tex_parameters ();
  Opengl.gl_teximage_2d ~bytes:ui_info.font_texture_atlas.bytes
    ~width:ui_info.font_texture_atlas.width
    ~height:ui_info.font_texture_atlas.height

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
  let Ui.{ width; height; x; y } = box.bbox
  and Ui.(r, g, b, alpha) = box.background_color in
  let points =
    [| (x, y + height); (x, y); (x + width, y); (x + width, y + height) |]
    |> Array.map (fun (x', y') -> (Float.of_int x', Float.of_int y'))
  in
  let window_width, window_height = Sdl.Sdl.sdl_gl_getdrawablesize () in
  let idx = ref 0 in
  Array.iteri
    (fun i (x, y) ->
      let x = (x /. (Float.of_int window_width /. 2.)) -. 1. in
      let y = (-.y /. (Float.of_int window_height /. 2.)) +. 1. in
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
    ~(glyph_info : Freetype.FreeType.glyph_info_) ~x ~y ~(glyph : char) =
  let window_width, window_height = Sdl.Sdl.sdl_get_window_size Sdl.Sdl.w in
  let window_width_gl, window_height_gl = Sdl.Sdl.sdl_gl_getdrawablesize () in
  let width_ratio = Float.of_int (window_width_gl / window_width) in
  let height_ratio = Float.of_int (window_height_gl / window_height) in
  let x_scaled, y_scaled =
    List.map (fun v -> Float.of_int v) [ x; y ] |> function
    | first :: second :: _ ->
        ( first /. Float.of_int window_width *. width_ratio,
          second /. Float.of_int window_height *. height_ratio )
    | _ -> failwith "failed to match list"
  in
  let width_scaled =
    Float.of_int glyph_info.width /. Float.of_int window_width *. width_ratio
  and height_scaled =
    Float.of_int glyph_info.rows /. Float.of_int window_height *. height_ratio
  in
  let config = Ui.get_ui_information () in
  let left, right, top, bottom =
    Render.get_tex_coords
      ~config:
        {
          glyph_info_with_char = config.glyph_info_with_char;
          font_glyph_texture_atlas_info = config.font_texture_atlas;
        }
      ~glyph ~glyph_info
  and horiBearing_Y_Scaled =
    Float.of_int glyph_info.horiBearingY
    /. Float.of_int window_height *. height_ratio
  and horiBearing_X_Scaled =
    Float.of_int glyph_info.horiBearingX
    /. Float.of_int window_width *. width_ratio
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

let config = Ui.get_ui_information ()

let rec draw_box ~(box : Ui.box) =
  write_container_values_to_ui_buffer ~box ~buffer:Render.Render.ui_buffer;
  draw_to_gl_buffer ();
  match box.content with
  | Some (Box b) ->
      let Ui.
            {
              left = box_left;
              right = box_right;
              top = box_top;
              bottom = box_bottom;
            } =
        Ui.get_box_sides ~box
      and Ui.{ left = b_left; right = b_right; top = b_top; bottom = b_bottom }
          =
        Ui.get_box_sides ~box:b
      in
      let new_box =
        {
          b with
          bbox =
            {
              x = max box_left b_left;
              y = max box_top b_top;
              width =
                (if
                   b_right > box_right
                   || Option.is_some box.take_remaining_space
                      && (Option.get box.take_remaining_space = Horizontal
                         || Option.get box.take_remaining_space = Both)
                 then box_right - b.bbox.x
                 else b.bbox.width);
              height =
                (if
                   b_bottom > box_bottom
                   || Option.is_some box.take_remaining_space
                      && (Option.get box.take_remaining_space = Vertical
                         || Option.get box.take_remaining_space = Both)
                 then box_bottom - b.bbox.y
                 else b.bbox.height);
            };
        }
      in
      draw_box ~box:new_box
  | Some (Boxes list) -> (
      match box.flow with
      | Some Horizontal ->
          let horizontal_pos = ref box.bbox.x in
          let new_boxes =
            List.map
              (fun b ->
                let new_box =
                  {
                    b with
                    Ui.bbox =
                      { b.Ui.bbox with x = !horizontal_pos; y = box.bbox.y };
                  }
                in
                horizontal_pos := !horizontal_pos + b.Ui.bbox.width;
                new_box)
              list
          in
          List.iter (fun b -> draw_box ~box:b) new_boxes
      | Some Vertical ->
          let vertical_pos = ref box.bbox.y in
          let new_boxes =
            List.map
              (fun b ->
                let new_box =
                  {
                    b with
                    Ui.bbox =
                      { b.Ui.bbox with y = !vertical_pos; x = box.bbox.x };
                  }
                in
                vertical_pos := !vertical_pos + b.Ui.bbox.height;
                new_box)
              list
          in
          List.iter (fun b -> draw_box ~box:b) new_boxes
      | Some Both -> ()
      | None -> List.iter (fun b -> draw_box ~box:b) list)
  | Some (Text s) ->
      let l = String.fold_right (fun c acc -> c :: acc) s [] in
      let horizontal_pos = ref box.bbox.x in
      List.iter
        (fun c ->
          let found =
            Array.find_opt (fun (c', _) -> c' = c) config.glyph_info_with_char
          in
          let c, glyph = Option.get found in
          write_to_text_buffer ~render_buf_container:Render.Render.ui_buffer
            ~glyph_info:glyph ~x:!horizontal_pos
            ~y:(box.bbox.y + config.font_height)
            ~glyph:c;
          draw_to_gl_buffer_text ();
          horizontal_pos := !horizontal_pos + glyph.Freetype.FreeType.x_advance)
        l
  | None -> ()

let draw ~(box : Ui.box) =
  Opengl.gl_clear_color 1. 1. 1. 1.;
  Opengl.gl_clear ();

  draw_box ~box;

  Sdl.Sdl.sdl_gl_swapwindow Sdl.Sdl.w
