#include <ft2build.h>
#include FT_FREETYPE_H
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "stubs.h"

CAMLprim value get_buffer_size(value buffer) {
  CAMLparam1(buffer);
  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);
  CAMLreturn(Val_int(b->size));
}

CAMLprim value init_buffer(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(buffer);
  buffer = caml_alloc(1, Abstract_tag);
  size_t c = 10000000; // 10MB roughly; this needs to be high enough so that no re-allocations are necessary
  float* contents = malloc(sizeof(float) * c);
  struct Buffer b = { .contents = contents, .size = 0, .capacity = c };
  *((struct Buffer**)Data_abstract_val(buffer)) = malloc(sizeof(struct Buffer));
  memcpy(*((struct Buffer**)Data_abstract_val(buffer)), &b, sizeof(struct Buffer));
  CAMLreturn(buffer);
}

void push_to_buffer(struct Buffer* b, const struct GlyphInfo glyph_info, int window_width, int window_height, int x_offset, int font_height) {

  float horiBearingX = glyph_info.horiBearingX / (float)window_width;
  float horiBearingY = glyph_info.horiBearingY / (float)window_height;

  // wrapping logic
  int next_advance = x_offset + glyph_info.x_advance;
  int diff_to_next_window_width_multiple = window_width - x_offset % window_width;
  int next_window_width_multiple = x_offset + diff_to_next_window_width_multiple;
  // printf("%d %d\n", next_advance, next_window_width_multiple);
  float x_offset_adjusted = next_advance > next_window_width_multiple ? (
    0
  ) : (x_offset % (next_window_width_multiple - window_width)) / (float)window_width;

  int row = (x_offset + glyph_info.x_advance) / window_width + 1;
  float font_height_norm = font_height / (float)window_height;
  float y_offset = row * font_height_norm;

  for (int buffer_idx = b->size; buffer_idx < b->size + glyph_info.buffer.size; buffer_idx += 3) {
    if (buffer_idx > b->capacity) caml_failwith("BUFFER TOO SMALL");

    int first = buffer_idx;
    int second = buffer_idx + 1;
    int third = buffer_idx + 2;

    // dividing by three because of FT_LOAD_TARGET_LCD
    float altered_x = glyph_info.buffer.contents[first - b->size] / 3 / window_width - 1.0 + x_offset_adjusted + horiBearingX;
    float altered_y = -glyph_info.buffer.contents[second - b->size] / window_height + 1.0 - y_offset + horiBearingY;

    b->contents[first] = altered_x;
    b->contents[second] = altered_y;
    b->contents[third] = glyph_info.buffer.contents[third - b->size] / 255.;
  }
  b->size += glyph_info.buffer.size;
}

/*
reset_buffer needs be called on the screen buffer when the screen needs to be redrawn
or else the buffer_idx (the starting index for where push_to_buffer starts) will overflow
*/
CAMLprim value reset_buffer(value buffer) {
  CAMLparam1(buffer);
  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);
  b->size = 0;
  CAMLreturn(Val_unit);
}
/*
previous_offset here is the summed x_advance of the glyphs up until this call.
Currently I don't know if there is a use for the vertical offsets or y_advances.
The x_advance sum is used to offset the new glyph correctly and draw on a new line if necessary.
*/
CAMLprim value write_to_buffer(value buffer, value glyph_info, value window_width, value window_height, value previous_offset, value font_height) {
  CAMLparam5(buffer, glyph_info, window_width, window_height, previous_offset);
  CAMLxparam1(font_height);

  int x_offset = Int_val(previous_offset);

  int font_height_c = Int_val(font_height);

  struct GlyphInfo* glyph_info_struct = *(struct GlyphInfo**)Data_abstract_val(glyph_info);

  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);

  push_to_buffer(b, *glyph_info_struct, Int_val(window_width), Int_val(window_height), x_offset, font_height_c);

  CAMLreturn(Val_unit);
}
