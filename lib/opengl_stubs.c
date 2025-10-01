#ifdef __APPLE__
#define GL_SILENCE_DEPRECATION
#include <OpenGL/gl.h>
#endif
#ifdef __linux__
#include <GL/glew.h>
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
#include "stubs.h"

CAMLprim value set_gl_tex_parameters_ui_text() {
  CAMLparam0();
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST_MIPMAP_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
  CAMLreturn(Val_unit);
}

CAMLprim value set_gl_tex_parameters() {
  CAMLparam0();
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_GENERATE_MIPMAP, GL_TRUE);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_set_viewport(value x_val, value y_val, value width_val, value height_val) {
  CAMLparam4(x_val, y_val, width_val, height_val);
  int x = Int_val(x_val);
  int y = Int_val(y_val);
  int width = Int_val(width_val);
  int height = Int_val(height_val);
  glViewport(x, y, width, height);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_get_viewport() {
  CAMLparam0();
  CAMLlocal1(four_tuple);
  int values[4];
  glGetIntegerv(GL_VIEWPORT, values);
  four_tuple = caml_alloc(4, 0);
  Store_field(four_tuple, 0, Val_int(values[0]));
  Store_field(four_tuple, 1, Val_int(values[1]));
  Store_field(four_tuple, 2, Val_int(values[2]));
  Store_field(four_tuple, 3, Val_int(values[3]));
  CAMLreturn(four_tuple);
}

CAMLprim value glew_init() {
  CAMLparam0();
#ifdef __linux__
  GLenum err = glewInit();
  if (GLEW_OK != err) { caml_failwith(glewGetErrorString(err)); }
#endif
  CAMLreturn(Val_unit);
}

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

CAMLprim value gl_uniform_1i(value location, value val) {
  CAMLparam2(location, val);
  glUniform1i(Int_val(location), Int_val(val));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_gen_texture() {
  CAMLparam0();
  GLuint texture;
  glGenTextures(1, &texture);
  CAMLreturn(Val_int(texture));
}

CAMLprim value gl_bind_texture(value texture_id) {
  CAMLparam1(texture_id);
  glBindTexture(GL_TEXTURE_2D, Int_val(texture_id));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_teximage_2d(value bytes, value width, value height) {
  CAMLparam3(bytes, width, height);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
  glTexImage2D(
    GL_TEXTURE_2D,
    0,
    GL_ALPHA,
    Int_val(width),
    Int_val(height),
    0,
    GL_ALPHA,
    GL_UNSIGNED_BYTE,
    Bytes_val(bytes)
  );
  CAMLreturn(Val_unit);
}

CAMLprim value gl_enable_texture_2d() {
  CAMLparam0();
  glEnable(GL_TEXTURE_2D);
  CAMLreturn(Val_unit);
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
  glBufferData(GL_ARRAY_BUFFER, b->capacity * sizeof(float), b->contents, GL_DYNAMIC_DRAW);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_buffer_data_big_array(value bigarray, value capacity) {
  CAMLparam2(bigarray, capacity);
  glBufferData(GL_ARRAY_BUFFER, Int_val(capacity) * sizeof(float), (float *)Caml_ba_data_val(bigarray), GL_DYNAMIC_DRAW);
  CAMLreturn(Val_unit);
}

CAMLprim value gl_buffer_subdata_big_array(value bigarray, value length) {
  CAMLparam2(bigarray, length);
  glBufferSubData(GL_ARRAY_BUFFER, 0, Int_val(length) * sizeof(float), (float *)Caml_ba_data_val(bigarray));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_buffer_subdata(value buffer) {
  CAMLparam1(buffer);
  struct Buffer* b = *(struct Buffer**)Data_abstract_val(buffer);
  glBufferSubData(GL_ARRAY_BUFFER, 0, b->size * sizeof(float), b->contents);
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

CAMLprim value gl_vertex_attrib_pointer_float_type(value location, value size, value stride, value normalized, value start_idx) {
  CAMLparam5(location, size, stride, normalized, start_idx);
  glVertexAttribPointer(Int_val(location), Int_val(size), GL_FLOAT, Bool_val(normalized), sizeof(float) * Int_val(stride), (void*)(Int_val(start_idx) * sizeof(float)));
  CAMLreturn(Val_unit);
}

CAMLprim value gl_draw_arrays_with_quads(value num_points) {
  CAMLparam1(num_points);
  glDrawArrays(GL_QUADS, 0, Int_val(num_points));
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

CAMLprim value gl_getuniformlocation(value program, value name) {
  CAMLparam2(program, name);
  CAMLlocal1(result);
  GLint location = glGetUniformLocation(Int_val(program), String_val(name));
  if (location == -1) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string("named attribute variable is not an active attribute or name starts with 'gl_'"));
    CAMLreturn(result);
  }
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_int(location));
  CAMLreturn(result);
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
