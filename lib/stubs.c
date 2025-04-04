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
  size_t c = 10000000; // 10MB roughly; this needs to be high enough so that no re-allocations are necessary
  float* contents = malloc(sizeof(float) * c);
  struct Buffer b = { .contents = contents, .size = 0, .capacity = c };
  *((struct Buffer**)Data_abstract_val(buffer)) = malloc(sizeof(struct Buffer));
  memcpy(*((struct Buffer**)Data_abstract_val(buffer)), &b, sizeof(struct Buffer));
  CAMLreturn(buffer);
}

void push_to_buffer(struct Buffer* b, const struct Buffer bitmap_buf, int window_width, int window_height) {
  for (int buffer_idx = b->size; buffer_idx < b->size + bitmap_buf.size; buffer_idx += 3) {
    if (buffer_idx > b->capacity) caml_failwith("BUFFER TOO SMALL");

    int first = buffer_idx;
    int second = buffer_idx + 1;
    int third = buffer_idx + 2;

    // dividing by three because of FT_LOAD_TARGET_LCD
    float altered_x = bitmap_buf.contents[first - b->size] / 3 / window_width;
    float altered_y = -bitmap_buf.contents[second - b->size] / window_height;

    b->contents[first] = altered_x;
    b->contents[second] = altered_y;
    b->contents[third] = bitmap_buf.contents[third - b->size] / 255.;
  }
  b->size += bitmap_buf.size;
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

CAMLprim value write_to_buffer(value buffer, value bitmap_buffer, value window_width, value window_height) {
  CAMLparam4(buffer, bitmap_buffer, window_width, window_height);

  struct Buffer* bitmap_buf = *(struct Buffer**)Data_abstract_val(bitmap_buffer);

  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);

  push_to_buffer(b, *bitmap_buf, Int_val(window_width), Int_val(window_height));

  CAMLreturn(Val_unit);
}
