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
#include "stubs.h"

CAMLprim value get_font_height(value face) {
  CAMLparam1(face);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);

  // using FT_Size_Metrics.height because it is expressed in fractional units as
  // opposed to font units so there isn't any conversion necessary besides just taking away
  // the fractional parts (>> 6).
  CAMLreturn(Val_int((*face_c)->size->metrics.height >> 6));
}

CAMLprim value get_horiBearingX(value glyph_info) {
  CAMLparam1(glyph_info);
  struct GlyphInfo* glyph_info_c = *(struct GlyphInfo**)Data_abstract_val(glyph_info);
  CAMLreturn(Val_int(glyph_info_c->horiBearingX));
}
CAMLprim value get_x_advance(value glyph_info) {
  CAMLparam1(glyph_info);
  struct GlyphInfo* glyph_info_c = *(struct GlyphInfo**)Data_abstract_val(glyph_info);
  CAMLreturn(Val_int(glyph_info_c->x_advance));
}

CAMLprim value get_ascii_char_glyph(value face, value ascii) {
  CAMLparam2(face, ascii);
  CAMLlocal1(glyph_info);
  CAMLlocal1(tuple);

  int ascii_value = Int_val(ascii);
  if (ascii_value < 32 || ascii_value > 126) caml_failwith("ascii value out of range");

  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);

  FT_UInt glyph_index = FT_Get_Char_Index(*face_c, ascii_value);
  int result = FT_Load_Glyph(*face_c, glyph_index, FT_LOAD_RENDER | FT_LOAD_TARGET_LCD);
  if (result) {
    caml_failwith("ascii FT_Load_Glyph failed");
  }

  glyph_info = caml_alloc(1, Abstract_tag);
  tuple = caml_alloc(2, 0);

  FT_Bitmap bitmap = (*face_c)->glyph->bitmap;

  int size = bitmap.rows * bitmap.width * 3;

  struct Buffer buf = { .contents = malloc(sizeof(float) * size), .size = size, .capacity = size };

  int buffer_idx = 0;
  for (int y = 0; y < bitmap.rows; ++y) {
    for (int x = 0; x < bitmap.width; ++x) {
      float alpha = bitmap.buffer[y * bitmap.pitch + x];
      buf.contents[buffer_idx] = x;
      buf.contents[buffer_idx + 1] = y;
      buf.contents[buffer_idx + 2] = alpha;
      // three because x,y,z for opengl vertex attrib array stride
      buffer_idx += 3;
    }
  }

  // shifting right by 6 because freetype uses 26.6 fixed floating point numbers
  struct GlyphInfo glyph_info_struct = {
    .horiBearingX = (*face_c)->glyph->metrics.horiBearingX >> 6,
    .horiBearingY = (*face_c)->glyph->metrics.horiBearingY >> 6,
    .x_advance = (*face_c)->glyph->advance.x >> 6,
    .y_advance = (*face_c)->glyph->advance.y >> 6
  };

  memcpy(&glyph_info_struct.buffer, &buf, sizeof(struct Buffer));

  *(struct Buffer**)Data_abstract_val(glyph_info) = malloc(sizeof(struct GlyphInfo));
  memcpy(*(struct Buffer**)Data_abstract_val(glyph_info), &glyph_info_struct, sizeof(struct GlyphInfo));

  Store_field(tuple, 0, ascii);
  Store_field(tuple, 1, glyph_info);

  CAMLreturn(tuple);
}

CAMLprim value freetype_set_char_size(value face, value size) {
  CAMLparam2(face, size);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  int result = FT_Set_Char_Size(*face_c, 0, Int_val(size), 0, 0);
  if (result) caml_failwith("FT_Set_Char_Size failed");
  CAMLreturn(Val_unit);
}

CAMLprim value freetype_set_pixel_sizes(value face, value size) {
  CAMLparam2(face, size);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  int result = FT_Set_Pixel_Sizes(*face_c, 0, Int_val(size));
  if (result) caml_failwith("FT_Set_Pixel_Sizes failed");
  CAMLreturn(Val_unit);
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

  *((FT_Face**)Data_abstract_val(abstract_face)) = malloc(sizeof(FT_Face));
  *((FT_Library**)Data_abstract_val(abstract_library)) = malloc(sizeof(FT_Library));

  memcpy(*((FT_Face**)Data_abstract_val(abstract_face)), &face, sizeof(FT_Face));
  memcpy(*((FT_Library**)Data_abstract_val(abstract_library)), &library, sizeof(FT_Library));

  tuple = caml_alloc(2, 0);
  Store_field(tuple, 0, abstract_face);
  Store_field(tuple, 1, abstract_library);
  CAMLreturn(tuple);
}
