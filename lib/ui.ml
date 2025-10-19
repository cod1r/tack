type bounding_box = {
  mutable width : int;
  mutable height : int;
  mutable x : int;
  mutable y : int;
}

type direction = Horizontal | Vertical
type box_sides = { left : int; right : int; top : int; bottom : int }
type positioning = Relative | Absolute
type horizontal_alignment = Left | Center | Right
type vertical_alignment = Top | Center | Bottom

type box = {
  mutable name : string option;
  mutable content : box_content option;
  mutable bbox : bounding_box option;
  mutable text_wrap : bool;
  mutable background_color : float * float * float * float;
  mutable border : bool;
  mutable flow : direction option;
  mutable take_remaining_space : direction option;
  mutable font_size : int option;
  mutable width_min_content : bool;
  mutable height_min_content : bool;
  mutable clip_content : bool;
  mutable position_type : positioning;
  mutable allow_horizontal_scroll : bool;
  mutable allow_vertical_scroll : bool;
  mutable horizontal_align : horizontal_alignment option;
  mutable vertical_align : vertical_alignment option;
  on_event : event_handler_t option;
}

and event_handler_t = b:box option -> e:Sdl.event -> unit

and text_area_information = {
  text : Rope.rope option;
  cursor_pos : int option;
  highlight_pos : int option * int option;
  holding_ctrl : bool;
  holding_mousedown_rope_pos : int option;
  scroll_x_offset : int;
  scroll_y_offset : int;
}

and box_content =
  | Box of box
  | Boxes of box list
  | Text of string
  | Textarea of text_area_information

let focused_element : box option ref = ref None
let set_focused_element ~(box : box) = focused_element := Some box
let unfocus_element () = focused_element := None
let default_bbox : bounding_box = { width = 0; height = 0; x = 0; y = 0 }

let default_box =
  {
    name = None;
    content = None;
    bbox = None;
    text_wrap = false;
    background_color = (1., 1., 1., 1.);
    border = false;
    take_remaining_space = None;
    font_size = None;
    width_min_content = false;
    height_min_content = false;
    clip_content = false;
    position_type = Relative;
    allow_horizontal_scroll = false;
    allow_vertical_scroll = false;
    horizontal_align = None;
    vertical_align = None;
    flow = None;
    on_event = None;
  }

type text_texture_atlas_info = { width : int; height : int; bytes : bytes }

type font_info = {
  glyph_info_with_char : (char * Freetype.glyph_info_) Array.t;
  font_size : int;
  font_texture_atlas : text_texture_atlas_info;
  font_height : int;
  ascender : int;
  descender : int;
}

let get_glyph_info_from_glyph ~glyph ~font_info =
  let opt =
    Array.find_opt (fun (c', _) -> c' = glyph) font_info.glyph_info_with_char
  in
  try
    let _, gi = Option.get opt in
    gi
  with Invalid_argument e -> failwith (__FUNCTION__ ^ "; " ^ e)

let get_text_wrap_info ~bbox ~glyph ~x ~y ~font_info =
  if glyph = '\n' then
    (~new_x:bbox.x, ~new_y:(y + font_info.font_height), ~wraps:true)
  else (
    let glyph_info = get_glyph_info_from_glyph ~glyph ~font_info in
    if x + glyph_info.x_advance > bbox.x + bbox.width then
      (~new_x:(bbox.x + glyph_info.x_advance),
      ~new_y:(y + font_info.font_height),
      ~wraps:true)
    else
      ~new_x:(x + glyph_info.x_advance), ~new_y:(y), ~wraps:false)

let clone_box ~(box : box) =
  let visited = ref [] in
  let rec clone_box' box =
    if List.exists (fun b -> b == box) !visited then
      failwith "Recursive structure detected"
    else visited := box :: !visited;
    {
      name = box.name;
      content =
        (match box.content with
        | Some (Box b) -> Some (Box (clone_box' b))
        | Some (Boxes list) ->
            Some (Boxes (List.map (fun b -> clone_box' b) list))
        | Some (Text _) | None -> box.content
        | Some (Textarea _) -> None);
      bbox = box.bbox;
      text_wrap = box.text_wrap;
      background_color = box.background_color;
      border = box.border;
      flow = box.flow;
      take_remaining_space = box.take_remaining_space;
      font_size = box.font_size;
      width_min_content = box.width_min_content;
      height_min_content = box.height_min_content;
      clip_content = box.clip_content;
      position_type = box.position_type;
      allow_vertical_scroll = box.allow_vertical_scroll;
      allow_horizontal_scroll = box.allow_horizontal_scroll;
      horizontal_align = box.horizontal_align;
      vertical_align = box.vertical_align;
      on_event = box.on_event;
    }
  in
  clone_box' box

let get_text_texture_atlas_info
    ~(glyph_info_with_char : (char * Freetype.glyph_info_) Array.t) =
  let face = Freetype.face in
  let descender = Freetype.get_descender face in
  let ascender = Freetype.get_ascender face in
  let widths_summed =
    Array.fold_left
      (fun acc (_, gi) -> acc + gi.Freetype.width)
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
  {
    (* width and height of text texture *)
    width = widths_summed;
    height = global_font_height;
    bytes = bytes_texture_atlas;
  }

let get_logical_to_opengl_window_dims_ratio () =
  let window_width, window_height = Sdl.sdl_get_window_size Sdl.w
  and window_width_gl, window_height_gl = Sdl.sdl_gl_getdrawablesize () in
  (window_width_gl / window_width, window_height_gl / window_height)

let get_new_font_info_with_font_size ~(font_size : int)
    ~(face : Freetype.ft_face) =
  let _, height_ratio = get_logical_to_opengl_window_dims_ratio () in
  (* scaling font_size appropriately based on ratio between the opengl window in pixels and the sdl window's pixels *)
  let font_size = font_size * height_ratio in
  let glyph_info_with_char =
    Array.init
      (126 - 32 + 1)
      (fun i -> Freetype.get_ascii_char_glyph_info_ face (i + 32) font_size)
  in
  let font_height = Freetype.get_font_height face in
  let descender = Freetype.get_descender face in
  let ascender = Freetype.get_ascender face in
  let text_texture_atlas_info =
    get_text_texture_atlas_info ~glyph_info_with_char
  in
  {
    glyph_info_with_char;
    font_size;
    font_texture_atlas = text_texture_atlas_info;
    font_height;
    ascender;
    descender;
  }

let get_box_sides ~(box : box) : box_sides =
  match box.bbox with
  | Some bbox ->
      let right = bbox.x + bbox.width and bottom = bbox.y + bbox.height in
      { left = bbox.x; top = bbox.y; right; bottom }
  | None -> failwith "calling get_box_sides requires a bbox property of Some"
