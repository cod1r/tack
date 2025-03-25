#ifdef __APPLE__
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

//CAMLprim value load_vertices(value bigarray, value size) {
//  glGenBuffers();
//  glBindBuffer();
//  glBufferData();
//  glVertexAttribPointer();
//}

CAMLprim value gl_clear_color(float r, float g, float b, float a) {
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

CAMLprim value gl_createshader(value type) {
  CAMLparam1(type);
  CAMLlocal1(result);
  GLenum shader_type = Int_val(type) == 0 ? GL_VERTEX_SHADER : GL_FRAGMENT_SHADER;
  GLuint shader = glCreateShader(shader_type);
  if (shader == 0) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string("shader couldn't be created"));
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
