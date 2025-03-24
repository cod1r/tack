#ifdef __APPLE__
#include <OpenGL/gl.h>
#endif
#ifdef __LINUX__
#include <GL/gl.h>
#endif
#ifdef __WINDOWS__
#include <gl/GL.h>
#endif

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

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
