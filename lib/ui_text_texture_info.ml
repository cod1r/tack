open Opengl
open Freetype

type texture_info =
  { gl_texture_id : int
  ; font_size : int
  ; font_info : Ui.font_info
  }

let text_textures_with_different_font_sizes : texture_info list ref = ref []

let get_or_add_font_size_text_texture ~(font_size : int) =
  let option =
    List.find_opt
      (fun { font_size = font_size'; _ } -> font_size' = font_size)
      !text_textures_with_different_font_sizes
  in
  match option with
  | Some { font_info; gl_texture_id; _ } -> ~font_info, ~gl_texture_id
  | None ->
    let gl_buffer_glyph_texture_atlas = Opengl.gl_gen_texture () in
    let font_info = Ui.get_new_font_info_with_font_size ~font_size ~face:Freetype.face in
    text_textures_with_different_font_sizes
    := { gl_texture_id = gl_buffer_glyph_texture_atlas; font_size; font_info }
       :: !text_textures_with_different_font_sizes;
    Opengl.gl_bind_texture ~texture_id:gl_buffer_glyph_texture_atlas;
    Opengl.set_gl_tex_parameters_ui_text ();
    Opengl.gl_teximage_2d
      ~bytes:font_info.font_texture_atlas.bytes
      ~width:font_info.font_texture_atlas.width
      ~height:font_info.font_texture_atlas.height;
    ~font_info, ~gl_texture_id:gl_buffer_glyph_texture_atlas
;;
