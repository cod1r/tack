#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <SDL.h>
#include <stdio.h>

CAMLprim value sdl_create_window(value title, value x, value y, value width, value height, value flags) {
  CAMLparam5(title, x, y, width, height);
  CAMLxparam1(flags);
  CAMLlocal1(window);
  CAMLlocal1(option_window);
  option_window = Val_none;
  SDL_Window* w = SDL_CreateWindow(String_val(title), Int_val(x), Int_val(y), Int_val(width), Int_val(height), Int_val(flags));
  if (w) {
    // tag type 0 because record type
    window = caml_alloc(5, 0);
    Store_field(window, 0, title);
    Store_field(window, 1, x);
    Store_field(window, 2, y);
    Store_field(window, 3, width);
    Store_field(window, 4, height);
    option_window = caml_alloc_some(window);
  }
  CAMLreturn(option_window);
}

CAMLprim value init_sdl(value unit) {
  CAMLparam1(unit);
  SDL_Init(SDL_INIT_VIDEO);
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
        Store_field(evt_type, 1, caml_copy_int64(e.key.timestamp));
        Store_field(evt_type, 2, caml_copy_int64(e.key.windowID));
        Store_field(evt_type, 3, e.key.state == SDL_PRESSED ? Val_int(0) : Val_int(1));
        Store_field(evt_type, 4, e.key.repeat ? Val_true : Val_false);
        Store_field(evt_type, 5, Val_int(e.key.keysym.sym));
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_MOUSEBUTTONDOWN: {
        // tag type is 1 because second variant
        evt_type = caml_alloc(7, 1);
        Store_field(evt_type, 0, Val_int(0));
        Store_field(evt_type, 1, caml_copy_int64(e.button.timestamp));
        Store_field(evt_type, 2, caml_copy_int64(e.button.windowID));
        Store_field(evt_type, 3, caml_copy_int32(e.button.button));
        Store_field(evt_type, 4, caml_copy_int32(e.button.clicks));
        Store_field(evt_type, 5, caml_copy_int64(e.button.x));
        Store_field(evt_type, 6, caml_copy_int64(e.button.y));
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_WINDOWEVENT: {
        if (e.window.event == SDL_WINDOWEVENT_CLOSE) {
          // tag type is 2 because third variant
          evt_type = caml_alloc(3, 2);
          Store_field(evt_type, 0, caml_copy_int64(e.window.timestamp));
          Store_field(evt_type, 1, caml_copy_int64(e.window.windowID));
          Store_field(evt_type, 2, Val_int(0));
          option_val = caml_alloc_some(evt_type);
        }
      } break;
    }
  }
  CAMLreturn(option_val);
}
