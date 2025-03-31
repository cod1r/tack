#ifdef __APPLE__
#define GL_SILENCE_DEPRECATION
#include <OpenGL/gl.h>
#endif
#ifdef __LINUX__
#include <GL/gl.h>
#endif
#ifdef __WINDOWS__
#include <gl/GL.h>
#endif

#include <ft2build.h>
#include FT_FREETYPE_H
#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include "stubs.h"

void check_error() {
  GLenum err = glGetError();
  switch (err) {
case GL_NO_ERROR:
     printf("No error has been recorded. The value of this symbolic constant is guaranteed to be 0.\n"); break;
case GL_INVALID_ENUM:
     caml_failwith("An unacceptable value is specified for an enumerated argument. The offending command is ignored and has no other side effect than to set the error flag."); break;
case GL_INVALID_VALUE:
     caml_failwith("A numeric argument is out of range. The offending command is ignored and has no other side effect than to set the error flag."); break;
case GL_INVALID_OPERATION:
     caml_failwith("The specified operation is not allowed in the current state. The offending command is ignored and has no other side effect than to set the error flag."); break;
case GL_STACK_OVERFLOW:
     caml_failwith("This command would cause a stack overflow. The offending command is ignored and has no other side effect than to set the error flag."); break;
case GL_STACK_UNDERFLOW:
     caml_failwith("This command would cause a stack underflow. The offending command is ignored and has no other side effect than to set the error flag."); break;
case GL_OUT_OF_MEMORY:
     caml_failwith("There is not enough memory left to execute the command. The state of the GL is undefined, except for the state of the error flags, after this error is recorded."); break;
case GL_TABLE_TOO_LARGE:
    caml_failwith("GL ERROR"); break;
  }
}

void push_to_buffer(struct Buffer* b, FT_Bitmap bitmap) {
  if (bitmap.rows * bitmap.width + b->size > b->capacity) caml_failwith("NEED TO HANDLE RESIZE BUFFER CASE");
  b->size += bitmap.rows * bitmap.width;
  for (int y = 0; y < bitmap.rows; ++y) {
    for (int x = 0; x < bitmap.width; ++x) {

    }
  }
}

CAMLprim value write_to_buffer(value buffer, value face, value letter, value idx_in_rope, value window_width) {
  CAMLparam5(buffer, face, letter, idx_in_rope, window_width);
  CAMLlocal1(tuple);
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

  int idx_in_buffer = Int_val(idx_in_rope);

  CAMLreturn(tuple);
}

CAMLprim value gl_enable_blending(value unit) {
  CAMLparam1(unit);
  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_enable_vertex_attrib_array(value location) {
  CAMLparam1(location);
  glEnableVertexAttribArray(Int_val(location));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_use_program(value program) {
  CAMLparam1(program);
  glUseProgram(Int_val(program));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_attach_shader(value program, value shader) {
  CAMLparam2(program, shader);
  glAttachShader(Int_val(program), Int_val(shader));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_shader_source(value shader, value source) {
  CAMLparam2(shader, source);
  const char* source_cstr = String_val(source);
  const GLint length = strlen(String_val(source));
  glShaderSource(Int_val(shader), 1, &source_cstr, &length);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_buffer_data(value buffer) {
  CAMLparam1(buffer);
  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);
  glBufferData(GL_ARRAY_BUFFER, b->size, b->contents, GL_DYNAMIC_DRAW);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_buffer_subdata(value buffer) {
  CAMLparam1(buffer);
  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, b->size, b->contents);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_get_shader_compile_status(value shader) {
  CAMLparam1(shader);
  GLint params;
  glGetShaderiv(Int_val(shader), GL_COMPILE_STATUS, &params);
  if (!params) CAMLreturn(Val_bool(false));
  CAMLreturn(Val_bool(true));
}

CAMLprim value gl_get_shader_info_log(value shader) {
  CAMLparam1(shader);
  CAMLlocal1(log);
  GLsizei length = 5000;
  char buffer[5000];
  memset(buffer, 0, length);
  glGetShaderInfoLog(Int_val(shader), 5000, &length, buffer);
  log = caml_copy_string(buffer);
  CAMLreturn(log);
}

CAMLprim value gl_linkprogram(value program) {
  CAMLparam1(program);
  glLinkProgram(Int_val(program));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_compileshader(value shader) {
  CAMLparam1(shader);
  glCompileShader(Int_val(shader));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_gen_one_buffer() {
  CAMLparam0();
  GLuint buffer;
  glGenBuffers(1, &buffer);
  CAMLreturn(Val_int(buffer));
}

CAMLprim value gl_vertex_attrib_pointer_float_type(value location, value size, value normalized) {
  CAMLparam3(location, size, normalized);
  glVertexAttribPointer(Int_val(location), Int_val(size), GL_FLOAT, Bool_val(normalized), sizeof(float) * Int_val(size), 0);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_draw_arrays(value num_points) {
  CAMLparam1(num_points);
  glDrawArrays(GL_POINTS, 0, Int_val(num_points));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_bind_buffer(value buffer) {
  CAMLparam1(buffer);
  glBindBuffer(GL_ARRAY_BUFFER, Int_val(buffer));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_clear_color(double r, double g, double b, double a) {
  CAMLparam0();
  glClearColor(r, g, b, a);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_clear(value unit) {
  CAMLparam1(unit);
  glClear(GL_COLOR_BUFFER_BIT);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_getattriblocation(value program, value name) {
  CAMLparam2(program, name);
  CAMLlocal1(result);
  GLint location = glGetAttribLocation(Int_val(program), String_val(name));
  if (location == -1) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string("named attribute variable is not an active attribute or name starts with 'gl_'"));
    CAMLreturn(result);
  }
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_int(location));
  CAMLreturn(result);
}

CAMLprim value gl_create_fragment_shader(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  GLenum shader_type = GL_FRAGMENT_SHADER;
  GLuint shader = glCreateShader(shader_type);
  if (shader == 0) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string("vertex shader couldn't be created"));
    CAMLreturn(result);
  }
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_int(shader));
  CAMLreturn(result);
}

CAMLprim value gl_create_vertex_shader(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  GLenum shader_type = GL_VERTEX_SHADER;
  GLuint shader = glCreateShader(shader_type);
  if (shader == 0) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string("vertex shader couldn't be created"));
    CAMLreturn(result);
  }
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_int(shader));
  CAMLreturn(result);
}

CAMLprim value gl_createprogram(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  GLuint program = glCreateProgram();
  if (program == 0) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string("program couldn't be created"));
    CAMLreturn(result);
  }
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_int(program));
  CAMLreturn(result);
}
