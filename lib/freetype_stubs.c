#include <ft2build.h>
#include FT_FREETYPE_H
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <SDL.h>
#include <hb.h>
#include <stdio.h>
#include <string.h>

CAMLprim value get_ascender(value face) {
  CAMLparam1(face);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  int ascender = (*face_c)->size->metrics.ascender >> 6;
  CAMLreturn(Val_int(ascender));
}

CAMLprim value get_descender(value face) {
  CAMLparam1(face);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  int descender = (*face_c)->size->metrics.descender >> 6;
  CAMLreturn(Val_int(descender));
}

CAMLprim value get_font_height(value face) {
  CAMLparam1(face);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);

  // using FT_Size_Metrics.height because it is expressed in fractional units as
  // opposed to font units so there isn't any conversion necessary besides just taking away
  // the fractional parts (>> 6).
  CAMLreturn(Val_int((*face_c)->size->metrics.height >> 6));
}

CAMLprim value freetype_set_char_size(value face, value size);
CAMLprim value freetype_set_pixel_sizes(value face, value size);
CAMLprim value get_ascii_char_glyph_info_(value face, value ascii, value size) {
  CAMLparam3(face, ascii, size);
  CAMLlocal1(glyph_info);
  CAMLlocal1(bytes);

  int ascii_value = Int_val(ascii);
  if (ascii_value < 32 || ascii_value > 126) caml_failwith("ascii value out of range");

  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);

  freetype_set_pixel_sizes(face, size);
  {
  int result = FT_Load_Char(*face_c, ascii_value, FT_LOAD_RENDER);
  if (result) {
    caml_failwith("ascii FT_Load_Glyph failed");
  }
  }

  glyph_info = caml_alloc(7, 0);

  FT_Bitmap bitmap = (*face_c)->glyph->bitmap;

  {
  int size = bitmap.rows * bitmap.width;

  bytes = caml_alloc_string(size);

  int buffer_idx = 0;
  for (int y = 0; y < bitmap.rows; ++y) {
    for (int x = 0; x < bitmap.width; ++x) {
      unsigned char alpha = bitmap.buffer[y * bitmap.pitch + x];
      Bytes_val(bytes)[buffer_idx++] = alpha;
    }
  }

  Store_field(glyph_info, 0, Val_int((*face_c)->glyph->metrics.horiBearingX >> 6));
  Store_field(glyph_info, 1, Val_int((*face_c)->glyph->metrics.horiBearingY >> 6));
  Store_field(glyph_info, 2, Val_int((*face_c)->glyph->advance.x >> 6));
  Store_field(glyph_info, 3, Val_int((*face_c)->glyph->advance.y >> 6));
  Store_field(glyph_info, 4, bytes);
  Store_field(glyph_info, 5, Val_int(bitmap.width));
  Store_field(glyph_info, 6, Val_int(bitmap.rows));
  }
  CAMLreturn(glyph_info);
}

CAMLprim value freetype_set_char_size(value face, value size) {
  CAMLparam2(face, size);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  int result = FT_Set_Char_Size(*face_c, 0, Int_val(size << 6), 0, 0);
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

CAMLprim value freetype_init_library() {
  CAMLparam0();
  CAMLlocal1(abstract_library);
  abstract_library = caml_alloc(1, Abstract_tag);

  FT_Library library;
  memset(&library, 0, sizeof(FT_Library));

  int result_init = FT_Init_FreeType(&library);
  if (result_init) caml_failwith("failed to initialize freetype");

  *((FT_Library**)Data_abstract_val(abstract_library)) = malloc(sizeof(FT_Library));
  memcpy(*((FT_Library**)Data_abstract_val(abstract_library)), &library, sizeof(FT_Library));

  CAMLreturn(abstract_library);
}

CAMLprim value freetype_get_face(value path_to_font, value ft_library) {
  CAMLparam2(path_to_font, ft_library);

  CAMLlocal1(abstract_face);
  abstract_face = caml_alloc(1, Abstract_tag);

  FT_Library* ft_library_c = *((FT_Library**)Data_abstract_val(ft_library));

  FT_Face face;
  memset(&face, 0, sizeof(FT_Face));

  printf("opening font file: %s\n", String_val(path_to_font));
  int result_face = FT_New_Face(*ft_library_c, String_val(path_to_font), 0, &face);
  if (result_face == FT_Err_Unknown_File_Format) {
    caml_failwith("unknown font file format");
  } else if (result_face) {
    caml_failwith("could not open font file for unknown reasons");
  }

  *((FT_Face**)Data_abstract_val(abstract_face)) = malloc(sizeof(FT_Face));

  memcpy(*((FT_Face**)Data_abstract_val(abstract_face)), &face, sizeof(FT_Face));

  CAMLreturn(abstract_face);
}

CAMLprim value freetype_done_face(value ft_face) {
  CAMLparam1(ft_face);
  FT_Face* ft_face_c = *((FT_Face**)Data_abstract_val(ft_face));
  int return_code = FT_Done_Face(*ft_face_c);
  if (return_code != 0) caml_failwith("FT_Done_Face failed");
  CAMLreturn(Val_unit);
}

CAMLprim value free_glyph_info(value glyph_info) {
  CAMLparam1(glyph_info);
  struct GlyphInfo* glyph_info_struct = *(struct GlyphInfo**)Data_abstract_val(glyph_info);
  free(glyph_info_struct);
  CAMLreturn(Val_unit);
}
