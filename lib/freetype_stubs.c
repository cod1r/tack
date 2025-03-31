#include <ft2build.h>
#include FT_FREETYPE_H
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <SDL.h>
#include <hb.h>
#include <stdio.h>
#include <string.h>

CAMLprim value freetype_set_pixel_sizes(value face, value size) {
  CAMLparam2(face, size);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  int result = FT_Set_Pixel_Sizes(*face_c, 0, Val_int(size));
  if (result) caml_failwith("FT_Set_Pixel_Sizes failed");
  CAMLreturn(Val_unit);
}

CAMLprim value freetype_set_char_size(value face, value unit) {
  CAMLparam2(face, unit);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  int result = FT_Set_Char_Size(*face_c, 0, 1*72, 800, 800);
  if (result) caml_failwith("FT_Set_Char_Size failed");
  CAMLreturn(Val_unit);
}

CAMLprim value freetype_load_glyph_letter(value face, value letter) {
  CAMLparam2(face, letter);
  CAMLlocal1(glyph);
  glyph = caml_alloc(1, Abstract_tag);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  FT_UInt glyph_index = FT_Get_Char_Index(*face_c, Int_val(letter));
  if (glyph_index == 0) caml_failwith("FT_Get_Char_Index returned undefined character code");
  int result = FT_Load_Glyph(*face_c, glyph_index, FT_LOAD_DEFAULT);
  if (result) {
    caml_failwith("FT_Load_Glyph failed");
  }
  int render_result = FT_Render_Glyph((*face_c)->glyph, FT_RENDER_MODE_LCD);
  if (render_result) caml_failwith("FT_Render_Glyph failed");
  **(FT_GlyphSlot**)Data_abstract_val(glyph) = (*face_c)->glyph;
  CAMLreturn(glyph);
}

CAMLprim value freetype_init(value path_to_font) {
  CAMLparam1(path_to_font);

  CAMLlocal1(tuple);
  CAMLlocal2(abstract_face, abstract_library);
  abstract_face = caml_alloc(1, Abstract_tag);
  abstract_library = caml_alloc(1, Abstract_tag);

  FT_Face face;
  FT_Library library;

  memset(&face, 0, sizeof(FT_Face));
  memset(&library, 0, sizeof(FT_Library));

  int result_init = FT_Init_FreeType(&library);
  if (result_init) caml_failwith("failed to initialize freetype");

  int result_face = FT_New_Face(library, "/System/Library/Fonts/Menlo.ttc", 0, &face);
  if (result_face == FT_Err_Unknown_File_Format) {
    caml_failwith("unknown font file format");
  } else if (result_face) {
    caml_failwith("could not open font file for unknown reasons");
  }
  **(FT_Face**)Data_abstract_val(abstract_face) = face;
  **(FT_Library**)Data_abstract_val(abstract_library) = library;

  tuple = caml_alloc(2, 0);
  Store_field(tuple, 0, abstract_face);
  Store_field(tuple, 1, abstract_library);
  CAMLreturn(tuple);
}
