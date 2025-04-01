#include <ft2build.h>
#include FT_FREETYPE_H
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "stubs.h"

CAMLprim value init_buffer(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(buffer);
  buffer = caml_alloc(1, Abstract_tag);
  size_t c = 10000000;
  float* contents = malloc(sizeof(float) * c);
  struct Buffer b = { .contents = contents, .size = 0, .capacity = c };
  *((struct Buffer**)Data_abstract_val(buffer)) = malloc(sizeof(struct Buffer));
  **((struct Buffer**)Data_abstract_val(buffer)) = b;
  CAMLreturn(buffer);
}

void push_to_buffer(struct Buffer* b, FT_Bitmap bitmap) {
  if (bitmap.rows * bitmap.width + b->size > b->capacity) caml_failwith("NEED TO HANDLE RESIZE BUFFER CASE");
  int buffer_idx = b->size;
  for (int y = 0; y < bitmap.rows; ++y) {
    for (int x = 0; x < bitmap.width; ++x) {
      uint8_t alpha = bitmap.buffer[y * bitmap.pitch + x];
      b->contents[buffer_idx] = x;
      b->contents[buffer_idx] = y;
      b->contents[buffer_idx] = alpha;
    }
  }
  b->size += bitmap.rows * bitmap.width;
}

CAMLprim value reset_buffer(value buffer) {
  CAMLparam1(buffer);
  struct Buffer* b = *(struct Buffer**)Data_abstract_val(b);
  b->size = 0;
  CAMLreturn(Val_unit);
}

CAMLprim value write_to_buffer(value buffer, value face, value letter, value window_width) {
  CAMLparam4(buffer, face, letter, window_width);
  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);
  FT_Face* face_c = *(FT_Face**)Data_abstract_val(face);
  char letter_c = Int_val(letter);

  FT_UInt glyph_index = FT_Get_Char_Index(*face_c, letter_c);
  if (glyph_index == 0) caml_failwith("FT_Get_Char_Index returned undefined character code");
  int result = FT_Load_Glyph(*face_c, glyph_index, FT_LOAD_DEFAULT);
  if (result) {
    caml_failwith("FT_Load_Glyph failed");
  }
  int render_result = FT_Render_Glyph((*face_c)->glyph, FT_RENDER_MODE_LCD);
  if (render_result) caml_failwith("FT_Render_Glyph failed");

  FT_GlyphSlot glyph = (*face_c)->glyph;

  push_to_buffer(b, glyph->bitmap);

  CAMLreturn(Val_unit);
}
