#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "stubs.h"

CAMLprim value init_buffer(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(buffer);
  size_t s = 10000000;
  float* contents = malloc(sizeof(float) * s);
  struct Buffer b = { .contents = contents, .size = s };
  **((struct Buffer**)Data_abstract_val(buffer)) = b;
  CAMLreturn(Val_unit);
}
