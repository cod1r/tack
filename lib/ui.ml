open Freetype

type bounding_box = { width : int; height : int; x : int; y : int }
type direction = Horizontal | Vertical | Both
type box_sides = { left : int; right : int; top : int; bottom : int }

type box = {
  mutable name : string option;
  mutable content : box_content option;
  mutable bbox : bounding_box;
  mutable text_wrap : bool;
  mutable background_color : float * float * float * float;
  mutable border : bool;
  mutable flow : direction option;
  mutable take_remaining_space : direction option;
}

and box_content = Box of box | Boxes of box list | Text of string

type texture_atlas_info = { width : int; height : int; bytes : bytes }

type ui_info = {
  glyph_info_with_char : (char * FreeType.glyph_info_) Array.t;
  font_path : string;
  font_size : int;
  font_texture_atlas : texture_atlas_info;
  font_height : int;
  ascender : int;
  descender : int;
}

let get_ui_information () =
  let config = Config.read_config () in
  let font_pixel_size =
    Yojson.Safe.Util.member "ui_font_pixel_size" config
    |> Yojson.Safe.Util.to_int
  and font_path =
    Yojson.Safe.Util.member "ui_font_path" config |> Yojson.Safe.Util.to_string
  in
  let face = FreeType.freetype_get_face font_path FreeType.library in
  FreeType.freetype_set_pixel_sizes face font_pixel_size;
  (* need to call font_height after set_pixel_sizes *)
  let font_height = FreeType.get_font_height face in
  let descender = FreeType.get_descender face in
  let ascender = FreeType.get_ascender face in
  let glyph_info_with_char =
    Array.init
      (126 - 32 + 1)
      (fun i -> FreeType.get_ascii_char_glyph_info_ face (i + 32))
  in
  let widths_summed =
    Array.fold_left
      (fun acc (_, gi) -> acc + gi.FreeType.width)
      0 glyph_info_with_char
  in
  let global_font_height = ascender - descender in
  let bytes_texture_atlas = Bytes.create (widths_summed * global_font_height) in
  (*
       the font glyph texture atlas is all of the glyphs that is loaded
       concatenated into a large bytes array

       widths of glyphs summed * font height
       ex:
         ABCDEF...
     *)
  let current_width = ref 0 in
  for glyph_info_index = 0 to Array.length glyph_info_with_char - 1 do
    let _, glyph_info = glyph_info_with_char.(glyph_info_index) in
    for row = 0 to glyph_info.rows - 1 do
      let slice =
        Bytes.sub glyph_info.bytes (row * glyph_info.width) glyph_info.width
      in
      Bytes.blit slice 0 bytes_texture_atlas
        (!current_width + (widths_summed * row))
        glyph_info.width
    done;
    current_width := !current_width + glyph_info.width
  done;
  let font_glyph_texture_atlas_info : texture_atlas_info =
    {
      width = widths_summed;
      height = global_font_height;
      bytes = bytes_texture_atlas;
    }
  in
  FreeType.freetype_done_face face;
  {
    glyph_info_with_char;
    font_size = font_pixel_size;
    font_path;
    font_texture_atlas = font_glyph_texture_atlas_info;
    font_height;
    ascender;
    descender;
  }

let get_box_sides ~(box : box) : box_sides =
  let right = box.bbox.x + box.bbox.width
  and bottom = box.bbox.y + box.bbox.height in
  { left = box.bbox.x; top = box.bbox.y; right; bottom }

let smol_box : box =
  {
    name = Some "test";
    background_color = (1., 1., 0., 1.);
    content = None;
    bbox = { width = 20; height = 20; x = 0; y = 0 };
    text_wrap = false;
    border = false;
    flow = None;
    take_remaining_space = None;
  }

let inner_box : box =
  {
    name = None;
    background_color = (0., 1., 0., 1.);
    content = Some (Boxes [ smol_box; smol_box ]);
    bbox = { width = 500; height = 50; x = 0; y = 0 };
    text_wrap = false;
    border = false;
    flow = Some Horizontal;
    take_remaining_space = None;
  }

let text_box : box =
  {
    name = None;
    background_color = (0., 1., 0., 0.8);
    content = Some (Text "urmomhig");
    bbox = { width = 500; height = 50; x = 0; y = 500 };
    text_wrap = false;
    border = false;
    flow = None;
    take_remaining_space = None;
  }

let box : box =
  {
    name = None;
    background_color = (1., 0., 0., 1.);
    content = Some (Boxes [ text_box ]);
    bbox = { width = 100; height = 100; x = 0; y = 0 };
    text_wrap = false;
    border = false;
    flow = Some Vertical;
    take_remaining_space = None;
  }

let smol_box_event_handler ~(e : Sdl.Sdl.event) =
  match e with
  | Sdl.Sdl.MouseMotionEvt { x; y; _ } ->
      let _, window_height = Sdl.Sdl.sdl_get_window_size Sdl.Sdl.w in
      let _, gl_window_height = Sdl.Sdl.sdl_gl_getdrawablesize () in
      let height_ratio = gl_window_height / window_height in
      let { left; top; right; bottom } = get_box_sides ~box:smol_box in
      if
        x >= left && x <= right
        && y >= top / height_ratio
        && y <= bottom / height_ratio
      then smol_box.background_color <- (1., 0., 1., 1.)
      else smol_box.background_color <- (1., 1., 0., 1.)
  | _ -> ()

let () = Ui_events.add_event_handler ~event_handler:smol_box_event_handler
