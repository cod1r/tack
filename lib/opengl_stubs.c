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

#include <caml/bigarray.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

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

CAMLprim value gl_buffer_data(value bigarray, value size) {
  CAMLparam2(bigarray, size);
  glBufferData(GL_ARRAY_BUFFER, Int_val(size), Caml_ba_data_val(bigarray), GL_DYNAMIC_DRAW);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_buffer_subdata(value bigarray, value offset, value size) {
  CAMLparam3(bigarray, offset, size);
  glBufferSubData(GL_ARRAY_BUFFER, Int_val(offset), Int_val(size), Caml_ba_data_val(bigarray));
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

CAMLprim value gl_gen_buffers(value num) {
  CAMLparam1(num);
  GLuint buffer;
  glGenBuffers(num, &buffer);
  CAMLreturn(buffer);
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
