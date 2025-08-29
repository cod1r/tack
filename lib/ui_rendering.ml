let gl_ui_lib_buffer = Opengl.gl_gen_one_buffer ()

let vertex_id =
  match Opengl.gl_create_vertex_shader () with
  | Ok v -> v
  | Error s -> failwith s

let fragment_id =
  match Opengl.gl_create_fragment_shader () with
  | Ok v -> v
  | Error s -> failwith s

let ui_program =
  Render.compile_shaders_and_return_program ~vertex_id ~fragment_id
    ~vertex_src:Render.generic_vertex_shader
    ~fragment_src:Render.generic_fragment_shader

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
  Array.iteri (fun i (x, y) ->
    let x = x /. (Float.of_int window_width /. 2.) -. 1. in
    let y = -.y /. (Float.of_int window_height /. 2.) +. 1. in
    buffer.buffer.{!idx} <- x;
    buffer.buffer.{!idx + 1} <- y;
    buffer.buffer.{!idx + 2} <- r;
    buffer.buffer.{!idx + 3} <- g;
    buffer.buffer.{!idx + 4} <- b;
    buffer.buffer.{!idx + 5} <- alpha;
    idx := (i + 1) * 6
  ) points;
  buffer.length <- 24

let () =
  Opengl.gl_bind_buffer gl_ui_lib_buffer;
  Opengl.gl_buffer_data_big_array ~render_buffer:Render.Render.ui_buffer.buffer
    ~capacity:(Bigarray.Array1.dim Render.Render.ui_buffer.buffer)

let draw_box ~(box : Ui.box) =
  write_container_values_to_ui_buffer ~box ~buffer:Render.Render.ui_buffer;
  match box.content with
  | Some (Box _) -> (
  )
  | Some (Boxes list) -> (
  )
  | Some (Text s) -> ()
  | None -> ()

let draw ~(box : Ui.box) =
  Opengl.gl_clear_color 1. 1. 1. 1.;
  Opengl.gl_clear ();

  draw_box ~box;

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

  Opengl.gl_draw_arrays_with_quads (Render.Render.ui_buffer.length / Render._EACH_POINT_FLOAT_AMOUNT);

  Bigarray.Array1.fill Render.Render.ui_buffer.buffer 0.;

  Render.Render.ui_buffer.length <- 0;

  Sdl.Sdl.sdl_gl_swapwindow Sdl.Sdl.w
