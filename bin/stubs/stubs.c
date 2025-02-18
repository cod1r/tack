#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <SDL2/SDL.h>
#include <stdio.h>

CAMLprim value init_sdl() {
  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindow("WTF", 0, 0, 800, 800, 0);
}

CAMLprim value sdl_pollevent(value sdl_event) {
  CAMLparam1(sdl_event);
  CAMLlocal1(evt_type);
  SDL_Event e;
  int poll_event_value = SDL_PollEvent(&e);
  if (poll_event_value == 1) {
    evt_type = caml_copy_int64(e.type);
    Store_field(sdl_event, 0, evt_type);
  }
  CAMLreturn(Val_unit);
}
