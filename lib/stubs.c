#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include <SDL.h>
#include <hb.h>
#include <stdio.h>

CAMLprim value sdl_render_present(value window) {
  CAMLparam1(window);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) caml_failwith(SDL_GetError());
  SDL_Renderer* renderer = SDL_GetRenderer(w);
  if (renderer == NULL) {
    caml_failwith("SDL_GetRenderer returned NULL trying to render present");
  }
  SDL_RenderPresent(renderer);
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_render_clear(value window) {
  CAMLparam1(window);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) caml_failwith(SDL_GetError());
  SDL_Renderer* renderer = SDL_GetRenderer(w);
  if (renderer == NULL) {
    caml_failwith("SDL_GetRenderer returned NULL trying to render clear");
  }
  int result = SDL_RenderClear(renderer);
  if (result < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_create_renderer(value window, value flags) {
  CAMLparam2(window, flags);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) caml_failwith(SDL_GetError());
  SDL_Renderer* r = SDL_CreateRenderer(w, -1, Int_val(flags));
  if (r == NULL) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_set_render_draw_color(value window, value r, value g, value b, value a) {
  CAMLparam5(window, r, g, b, a);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) caml_failwith(SDL_GetError());
  SDL_Renderer* renderer = SDL_GetRenderer(w);
  if (renderer == NULL) {
    caml_failwith("SDL_GetRenderer returned NULL trying to set draw color");
  }
  int res = SDL_SetRenderDrawColor(renderer, Int_val(r), Int_val(g), Int_val(b), Int_val(a));
  if (res < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_renderer_fill_rect(value window, value rect) {
  CAMLparam2(window, rect);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) caml_failwith(SDL_GetError());
  SDL_Renderer* r = SDL_GetRenderer(w);
  if (r == NULL) caml_failwith("SDL_GetRenderer returned NULL trying to fill rect");
  SDL_Rect c_rect = { Field(rect, 0), Field(rect, 1), Field(rect, 2), Field(rect, 3) };
  int res = SDL_RenderFillRect(r, &c_rect);
  if (res < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_renderer_draw_rect(value window, value rect) {
  CAMLparam2(window, rect);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) caml_failwith(SDL_GetError());
  SDL_Renderer* r = SDL_GetRenderer(w);
  if (r == NULL) caml_failwith("SDL_GetRenderer returned NULL trying to draw rect");
  SDL_Rect c_rect = { Field(rect, 0), Field(rect, 1), Field(rect, 2), Field(rect, 3) };
  int res = SDL_RenderDrawRect(r, &c_rect);
  if (res < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_create_window(value title, value x, value y, value width, value height, value flags) {
  CAMLparam5(title, x, y, width, height);
  CAMLxparam1(flags);
  CAMLlocal1(window);
  CAMLlocal1(option_window);
  option_window = Val_none;
  SDL_Window* w = SDL_CreateWindow(String_val(title), Int_val(x), Int_val(y), Int_val(width), Int_val(height), Int_val(flags));
  if (w) {
    // tag type 0 because record type
    window = caml_alloc(6, 0);
    Store_field(window, 0, Val_int(SDL_GetWindowID(w)));
    Store_field(window, 1, title);
    Store_field(window, 2, x);
    Store_field(window, 3, y);
    Store_field(window, 4, width);
    Store_field(window, 5, height);
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
        Store_field(evt_type, 1, Val_int(e.key.timestamp));
        Store_field(evt_type, 2, Val_int(e.key.windowID));
        Store_field(evt_type, 3, e.key.state == SDL_PRESSED ? Val_int(0) : Val_int(1));
        Store_field(evt_type, 4, e.key.repeat ? Val_true : Val_false);
        Store_field(evt_type, 5, Val_int(e.key.keysym.sym));
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_MOUSEBUTTONDOWN: {
        // tag type is 1 because second variant
        evt_type = caml_alloc(7, 1);
        Store_field(evt_type, 0, Val_int(0));
        Store_field(evt_type, 1, Val_int(e.button.timestamp));
        Store_field(evt_type, 2, Val_int(e.button.windowID));
        Store_field(evt_type, 3, Val_int(e.button.button));
        Store_field(evt_type, 4, Val_int(e.button.clicks));
        Store_field(evt_type, 5, Val_int(e.button.x));
        Store_field(evt_type, 6, Val_int(e.button.y));
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_WINDOWEVENT: {
        if (e.window.event == SDL_WINDOWEVENT_CLOSE) {
          // tag type is 2 because third variant
          evt_type = caml_alloc(3, 2);
          Store_field(evt_type, 0, Val_int(e.window.timestamp));
          Store_field(evt_type, 1, Val_int(e.window.windowID));
          Store_field(evt_type, 2, Val_int(0));
          option_val = caml_alloc_some(evt_type);
        }
      } break;
      case SDL_MOUSEMOTION: {
        evt_type = caml_alloc(7, 3);
        Store_field(evt_type, 0, Val_int(e.motion.timestamp));
        Store_field(evt_type, 1, Val_int(e.motion.windowID));
        Store_field(evt_type, 2, Val_int(e.motion.which));
        Store_field(evt_type, 3, Val_int(e.motion.x));
        Store_field(evt_type, 4, Val_int(e.motion.y));
        Store_field(evt_type, 5, Val_int(e.motion.xrel));
        Store_field(evt_type, 6, Val_int(e.motion.yrel));
        option_val = caml_alloc_some(evt_type);
      }
    }
  }
  CAMLreturn(option_val);
}
