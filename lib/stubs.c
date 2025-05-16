#include <ft2build.h>
#include FT_FREETYPE_H
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <stdlib.h>
#include <stdbool.h>
#include "stubs.h"

CAMLprim value get_buffer_size(value buffer) {
  CAMLparam1(buffer);
  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);
  CAMLreturn(Val_int(b->size));
}

CAMLprim value init_buffer(value floats_per_point) {
  CAMLparam1(floats_per_point);
  CAMLlocal1(buffer);
  buffer = caml_alloc(1, Abstract_tag);
  size_t c = 5000000 * Int_val(floats_per_point);
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

/*
 * The coordinate system is assumed to have 0,0 at the top left corner and goes to window_width,window_height
 *
 * then numbers are scaled to the opengl default viewport which is 0,0 in the center and goes to 1 for all sides
 */
void write_glyph_to_text_buffer(
  const struct GlyphInfo glyph_info,
  struct Buffer* text_buffer_c,
  int initial_x_offset,
  int initial_y_offset,
  int window_width,
  int window_height
) {
  int start = text_buffer_c->size;
  for (int buffer_idx = start; buffer_idx < start + glyph_info.buffer.size; buffer_idx += 3) {
    if (buffer_idx > text_buffer_c->capacity) caml_failwith("BUFFER TOO SMALL");

    int first = buffer_idx;
    int second = buffer_idx + 1;
    int third = buffer_idx + 2;

    // dividing by three because of FT_LOAD_TARGET_LCD
    // and adding in necessary offsets
    float altered_x = (glyph_info.buffer.contents[first - start] / 3 + initial_x_offset + glyph_info.horiBearingX);
    // 0 to 1 horizontally is half of the width of the screen due to 0 being in the center so I
    // scale by half the width of the screen so that full text width will go from 0 to 2.
    // Then subtract by 1 to offset x to position things to the furthest left position.
    altered_x /= (window_width / 2);
    altered_x -= 1.0;

    // negating (y float value plus row * font_height) because y float value is positive for going down and so is row * font_height
    // and our gl viewport is positive for going up.
    float altered_y = -(glyph_info.buffer.contents[second - start] + initial_y_offset) + glyph_info.horiBearingY;
    // scaling for the same reasons that altered_x is scaled.
    altered_y /= (window_height / 2);
    altered_y += 1.0;

    text_buffer_c->contents[text_buffer_c->size++] = altered_x;
    text_buffer_c->contents[text_buffer_c->size++] = altered_y;
    text_buffer_c->contents[text_buffer_c->size++] = 0.0f;
    text_buffer_c->contents[text_buffer_c->size++] = 0.0f;
    float alpha_value = glyph_info.buffer.contents[third - start] / 255.;
    text_buffer_c->contents[text_buffer_c->size++] = 0.0f;
    text_buffer_c->contents[text_buffer_c->size++] = alpha_value;
  }
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

CAMLprim value write_cursor_to_buffer(value buffer, value window_dims, value x_offset, value y_offset, value cursor_width, value cursor_height) {
  CAMLparam5(buffer, window_dims, x_offset, y_offset, cursor_width);
  CAMLxparam1(cursor_height);

  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);

  int window_width = Int_val(Field(window_dims, 0));
  int window_height = Int_val(Field(window_dims, 1));

  int x_offset_c = Int_val(x_offset);

  int y_offset_c = Int_val(y_offset);

  int cursor_width_c = Int_val(cursor_width);
  int cursor_height_c = Int_val(cursor_height);

  if (b->size != 0) {
    caml_failwith("b->size should be zero for the cursor buffer");
  }
  for (int x = 0; x < cursor_width_c; ++x) {
    for (int y = 0; y < cursor_height_c; ++y) {
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL FOR CURSOR");
      }
      b->contents[b->size++] = (x + x_offset_c) / ((float)window_width / 2) - 1;
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL FOR CURSOR");
      }
      b->contents[b->size++] = -(y + y_offset_c) / ((float)window_height / 2) + 1;
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL FOR CURSOR");
      }
      b->contents[b->size++] = 0.f;
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL FOR CURSOR");
      }
      b->contents[b->size++] = 0.f;
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL FOR CURSOR");
      }
      b->contents[b->size++] = 1.f;
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL FOR CURSOR");
      }
      b->contents[b->size++] = 0.5f;
      if (b->size > b->capacity) {
        caml_failwith("BUFFER TOO SMALL FOR CURSOR");
      }
    }
  }
  CAMLreturn(Val_unit);
}

CAMLprim value write_to_highlight_buffer(value highlight_buffer, value x, value y, value window_width, value window_height) {
  CAMLparam5(highlight_buffer, x, y, window_width, window_height);

  int x_c = Int_val(x);
  int y_c = Int_val(y);

  int window_width_c = Int_val(window_width);
  int window_height_c = Int_val(window_height);

  float x_scaled = x_c / (window_width_c / 2.f) - 1.f;
  float y_scaled = -y_c / (window_height_c / 2.f) + 1.f;

  struct Buffer* highlight_buffer_c = *(struct Buffer**)Data_abstract_val(highlight_buffer);

  highlight_buffer_c->contents[highlight_buffer_c->size++] = x_scaled;
  highlight_buffer_c->contents[highlight_buffer_c->size++] = y_scaled;
  highlight_buffer_c->contents[highlight_buffer_c->size++] = 0.f;
  highlight_buffer_c->contents[highlight_buffer_c->size++] = 0.f;
  highlight_buffer_c->contents[highlight_buffer_c->size++] = 1.f;
  highlight_buffer_c->contents[highlight_buffer_c->size++] = 0.5f;

  CAMLreturn(Val_unit);
}

CAMLprim value write_mouse_hover_to_highlight_buffer(value highlight, value window_width_val, value window_height_val, value font_height_val, value mouse_y_val) {
  CAMLparam5(highlight, window_width_val, window_height_val, font_height_val, mouse_y_val);

  struct Buffer* highlight_buffer_c = *(struct Buffer**)Data_abstract_val(highlight);

  int mouse_y = Int_val(mouse_y_val);
  int window_height = Int_val(window_height_val);
  int font_height = Int_val(font_height_val);

  int lower_y = mouse_y / font_height * font_height;
  int upper_y = (mouse_y + font_height) / font_height * font_height;

  CAMLreturn(Val_unit);
}

CAMLprim value write_search_to_text_buffer(value text_buffer, value glyph_info, value x_offset, value window_width, value window_height, value font_height_val) {
  CAMLparam5(text_buffer, glyph_info, x_offset, window_width, window_height);
  CAMLxparam1(font_height_val);
  struct Buffer* text_buffer_c = *(struct Buffer**)Data_abstract_val(text_buffer);
  struct GlyphInfo* glyph_info_struct = *(struct GlyphInfo**)Data_abstract_val(glyph_info);
  int x_offset_c = Int_val(x_offset);
  int font_height = Int_val(font_height_val);
  write_glyph_to_text_buffer(*glyph_info_struct, text_buffer_c, x_offset_c, font_height, Int_val(window_width), Int_val(window_height));
  CAMLreturn(Val_unit);
}

CAMLprim value write_glyph_to_text_buffer_value(value text_buffer, value glyph_info, value initial_x_offset_val, value initial_y_offset_val, value window_width_val, value window_height_val) {
  CAMLparam5(text_buffer, glyph_info, initial_x_offset_val, initial_y_offset_val, window_width_val);
  CAMLxparam1(window_height_val);

  int x_offset = Int_val(initial_x_offset_val);
  int y_offset = Int_val(initial_y_offset_val);

  int window_width = Int_val(window_width_val);
  int window_height = Int_val(window_height_val);

  struct GlyphInfo* glyph_info_struct = *(struct GlyphInfo**)Data_abstract_val(glyph_info);
  struct Buffer* text_buffer_c = *(struct Buffer**)Data_abstract_val(text_buffer);

  write_glyph_to_text_buffer(*glyph_info_struct, text_buffer_c, x_offset, y_offset, window_width, window_height);

  CAMLreturn(Val_unit);
}
