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

let write_values_to_ui_buffer ~(buffer: Render.render_buffer_wrapper) = ()

let draw_box ~(box : Ui.box) =
  match box.content with Box _ -> () | Boxes _ -> () | Text s -> ()

let draw ~(box : Ui.box) =
  Opengl.gl_clear_color 1. 1. 1. 1.;
  Opengl.gl_clear ();

  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:Render.Render.location_point_vertex ~size:2
    ~stride:Render._EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:0;

  Opengl.gl_vertex_attrib_pointer_float_type
    ~location:Render.Render.location_color ~size:4
    ~stride:Render._EACH_POINT_FLOAT_AMOUNT ~normalized:false ~start_idx:2
