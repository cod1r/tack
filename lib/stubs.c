#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include "stubs.h"

CAMLprim value init_buffer(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(buffer);
  size_t c = 10000000;
  float* contents = malloc(sizeof(float) * c);
  struct Buffer b = { .contents = contents, .size = 0, .capacity = c };
  **((struct Buffer**)Data_abstract_val(buffer)) = b;
  CAMLreturn(Val_unit);
}
