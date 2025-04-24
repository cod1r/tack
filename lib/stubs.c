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
  size_t c = 5000000 * 3; // 60MB (bc times sizeof(float)) roughly; this needs to be high enough so that no re-allocations are necessary; times 3 bc each point/pixel has 3 components (x,y,alpha)
  float* contents = malloc(c * sizeof(float));
  struct Buffer b = { .contents = contents, .size = 0, .capacity = c };
  *((struct Buffer**)Data_abstract_val(buffer)) = malloc(sizeof(struct Buffer));
  memcpy(*((struct Buffer**)Data_abstract_val(buffer)), &b, sizeof(struct Buffer));
  CAMLreturn(buffer);
}

CAMLprim value init_buffer_with_capacity(value size) {
  CAMLparam1(size);
  CAMLlocal1(buffer);
  buffer = caml_alloc(1, Abstract_tag);
  int size_c = Int_val(size);
  float* contents = malloc(size_c * sizeof(float));
  struct Buffer b = { .contents = contents, .size = 0, .capacity = size_c };
  *((struct Buffer**)Data_abstract_val(buffer)) = malloc(sizeof(struct Buffer));
  memcpy(*((struct Buffer**)Data_abstract_val(buffer)), &b, sizeof(struct Buffer));
  CAMLreturn(buffer);
}

int get_proper_x_offset(int original_x_offset, const struct GlyphInfo gi, int window_width) {
  int with_x_advance = (original_x_offset + gi.x_advance) / window_width;
  if (with_x_advance > original_x_offset / window_width) {
    original_x_offset = with_x_advance * window_width;
  }
  return original_x_offset;
}

void push_to_buffer(struct Buffer* b, const struct GlyphInfo glyph_info, int window_width, int window_height, int x_offset, int font_height) {

  int row = (x_offset + glyph_info.x_advance) / window_width + 1;

  int new_x_offset = get_proper_x_offset(x_offset, glyph_info, window_width);
  int used_x_offset = new_x_offset % window_width;

  for (int buffer_idx = b->size; buffer_idx < b->size + glyph_info.buffer.size; buffer_idx += 3) {
    if (buffer_idx > b->capacity) caml_failwith("BUFFER TOO SMALL");

    int first = buffer_idx;
    int second = buffer_idx + 1;
    int third = buffer_idx + 2;

    // dividing by three because of FT_LOAD_TARGET_LCD
    // and adding in necessary offsets
    float altered_x = (glyph_info.buffer.contents[first - b->size] / 3 + used_x_offset + glyph_info.horiBearingX);
    // 0 to 1 horizontally is half of the width of the screen due to 0 being in the center so I
    // scale by half the width of the screen so that full text width will go from 0 to 2.
    // Then subtract by 1 to offset x to position things to the furthest left position.
    altered_x /= (window_width / 2);
    altered_x -= 1.0;

    // negating (y float value plus row * font_height) because y float value is positive for going down and so is row * font_height
    // and our gl viewport is positive for going up.
    float altered_y = -(glyph_info.buffer.contents[second - b->size] + row * font_height) + glyph_info.horiBearingY;
    // scaling for the same reasons that altered_x is scaled.
    altered_y /= (window_height / 2);
    altered_y += 1.0;

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
CAMLprim value write_to_buffer(value buffer, value glyph_info, value window_dims, value previous_offset, value font_height) {
  CAMLparam5(buffer, glyph_info, window_dims, previous_offset, font_height);

  int window_width = Int_val(Field(window_dims, 0));

  int window_height = Int_val(Field(window_dims, 1));

  int x_offset = Int_val(previous_offset);

  int font_height_c = Int_val(font_height);

  struct GlyphInfo* glyph_info_struct = *(struct GlyphInfo**)Data_abstract_val(glyph_info);

  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);

  push_to_buffer(b, *glyph_info_struct, window_width, window_height, x_offset, font_height_c);

  CAMLreturn(Val_unit);
}

CAMLprim value write_cursor_to_buffer(value buffer, value window_dims, value previous_offset, value font_height) {
  CAMLparam4(buffer, window_dims, previous_offset, font_height);

  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);

  int window_width = Int_val(Field(window_dims, 0));
  int window_height = Int_val(Field(window_dims, 1));

  int x_offset = Int_val(previous_offset);
  int used_x_offset = x_offset % window_width;

  int font_height_c = Int_val(font_height);

  int row = x_offset / window_width;

  if (b->size != 0) {
    caml_failwith("b->size should be zero for the cursor buffer");
  }
  for (int x = 0; x < 2; ++x) {
    for (int y = 0; y < font_height_c; ++y) {
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL");
      }
      b->contents[b->size++] = (x + used_x_offset) / ((float)window_width / 2) - 1;
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL");
      }
      b->contents[b->size++] = -(y + row * font_height_c + 0.30 * font_height_c) / ((float)window_height / 2) + 1;
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL");
      }
      b->contents[b->size++] = 1;
    }
  }
  CAMLreturn(Val_unit);
}
