#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <SDL2/SDL.h>
#include <stdio.h>

CAMLprim value init_sdl(value unit) {
  CAMLparam1(unit);
  SDL_Init(SDL_INIT_VIDEO);
  SDL_CreateWindow("WTF HEHE XD", 0, 0, 800, 800, SDL_WINDOW_RESIZABLE);
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_pollevent(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(evt_type);
  CAMLlocal1(option_val);
  option_val = Val_none;
  SDL_Event e;
  int poll_event_value = SDL_PollEvent(&e);
  if (poll_event_value == 1) {
    switch (e.type) {
      case SDL_KEYDOWN: {
        // tag type is 0 because first variant
        evt_type = caml_alloc(6, 0);
        Store_field(evt_type, 0, Val_int(0));
        Store_field(evt_type, 1, Val_long(e.key.timestamp));
        Store_field(evt_type, 2, Val_long(e.key.windowID));
        Store_field(evt_type, 3, e.key.state == SDL_PRESSED ? Val_int(0) : Val_int(1));
        Store_field(evt_type, 4, e.key.repeat ? Val_true : Val_false);
        Store_field(evt_type, 5, Val_int(e.key.keysym.sym));
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_MOUSEBUTTONDOWN: {
        // tag type is 1 because second variant
        evt_type = caml_alloc(7, 1);
        Store_field(evt_type, 0, Val_int(0));
        Store_field(evt_type, 1, Val_long(e.button.timestamp));
        Store_field(evt_type, 2, Val_long(e.button.windowID));
        Store_field(evt_type, 3, Val_int(e.button.button));
        Store_field(evt_type, 4, Val_int(e.button.clicks));
        Store_field(evt_type, 5, Val_long(e.button.x));
        Store_field(evt_type, 6, Val_long(e.button.y));
        option_val = caml_alloc_some(evt_type);
      } break;
    }
  }
  CAMLreturn(option_val);
}
