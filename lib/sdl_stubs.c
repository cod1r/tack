#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <ft2build.h>
#include FT_FREETYPE_H
#include <SDL.h>
#include <hb.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>


CAMLprim value sdl_setup_macos_menu_bar() {
  CAMLparam0();
  #ifdef __APPLE__
  extern void setup_macos_menu_bar();
  setup_macos_menu_bar();
  #endif
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_getticks() {
  CAMLparam0();
  uint32_t ticks = SDL_GetTicks();
  CAMLreturn(Val_int(ticks));
}

CAMLprim value set_clipboard_text(value string) {
  CAMLparam1(string);
  int res = SDL_SetClipboardText(String_val(string));
  if (res) {
    caml_failwith(SDL_GetError());
  }
  CAMLreturn(Val_unit);
}

CAMLprim value get_clipboard_text() {
  CAMLparam0();
  CAMLlocal1(clip_text);
  char* text = SDL_GetClipboardText();
  clip_text = caml_copy_string(text);
  SDL_free(text);
  CAMLreturn(clip_text);
}

CAMLprim value sdl_create_and_set_system_cursor(value unit) {
  CAMLparam1(unit);
  SDL_Cursor* c = SDL_CreateSystemCursor(SDL_SYSTEM_CURSOR_IBEAM);
  SDL_SetCursor(c);
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_gl_setswapinterval(value interval) {
  CAMLparam1(interval);
  int ivl = Int_val(interval);
  int res = SDL_GL_SetSwapInterval(ivl);
  if (res == -1) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_gl_getswapinterval() {
  CAMLparam0();
  CAMLreturn(Val_int(SDL_GL_GetSwapInterval()));
}

CAMLprim value sdl_gl_swapwindow(value window) {
  CAMLparam1(window);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) {
    caml_failwith(SDL_GetError());
  }
  SDL_GL_SwapWindow(w);
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_gl_make_current(value window) {
  CAMLparam1(window);
  CAMLlocal1(result);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string(SDL_GetError()));
    CAMLreturn(result);
  }
  SDL_GLContext ctx = SDL_GL_GetCurrentContext();
  if (ctx == NULL) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string(SDL_GetError()));
    CAMLreturn(result);
  }
  int res = SDL_GL_MakeCurrent(w, ctx);
  if (res) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string(SDL_GetError()));
    CAMLreturn(result);
  }
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_unit);
  CAMLreturn(result);
}

CAMLprim value sdl_gl_create_context(value window) {
  CAMLparam1(window);
  CAMLlocal1(result);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string(SDL_GetError()));
    CAMLreturn(result);
  }
  SDL_GLContext ctx = SDL_GL_CreateContext(w);
  if (ctx == NULL) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string(SDL_GetError()));
    CAMLreturn(result);
  }
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_unit);
  CAMLreturn(result);
}

SDL_Renderer* get_renderer_from_window(value window) {
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) caml_failwith(SDL_GetError());
  SDL_Renderer* renderer = SDL_GetRenderer(w);
  if (renderer == NULL) {
    caml_failwith(SDL_GetError());
  }
  return renderer;
}

CAMLprim value sdl_gl_getdrawablesize(value unit) {
  CAMLparam1(unit);
  SDL_Window* window = SDL_GL_GetCurrentWindow();
  int w, h;
  SDL_GL_GetDrawableSize(window, &w, &h);
  uint64_t wh = ((uint64_t)w << 32) | h;
  CAMLreturn(Val_long(wh));
}

CAMLprim value sdl_get_renderer_size(value window) {
  CAMLparam1(window);
  CAMLlocal1(size_tuple);
  SDL_Renderer* r = get_renderer_from_window(window);
  int w;
  int h;
  int res = SDL_GetRendererOutputSize(r, &w, &h);
  if (res != 0) caml_failwith(SDL_GetError());
  size_tuple = caml_alloc(2, 0);
  Store_field(size_tuple, 0, Val_int(w));
  Store_field(size_tuple, 1, Val_int(h));
  CAMLreturn(size_tuple);
}

CAMLprim value sdl_get_window_size(value window) {
  CAMLparam1(window);
  CAMLlocal1(size_tuple);
  SDL_Window* w = SDL_GetWindowFromID(Int_val(Field(window, 0)));
  if (w == NULL) caml_failwith(SDL_GetError());
  int width, height;
  SDL_GetWindowSize(w, &width, &height);
  size_tuple = caml_alloc(2, 0);
  Store_field(size_tuple, 0, Val_int(width));
  Store_field(size_tuple, 1, Val_int(height));
  CAMLreturn(size_tuple);
}

CAMLprim value sdl_set_render_draw_blendmode(value window, value blendmode) {
  CAMLparam2(window, blendmode);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  int result = SDL_SetRenderDrawBlendMode(renderer, Int_val(blendmode));
  if (result) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_render_draw_point_f(value window, double x, double y) {
  CAMLparam1(window);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  int result = SDL_RenderDrawPointF(renderer, x, y);
  if (result) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_render_draw_point_f_bytec(value window, value x, value y) {
  CAMLparam3(window, x, y);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  int result = SDL_RenderDrawPointF(renderer, Double_val(x), Double_val(y));
  if (result) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_render_draw_points_float(value window, value points_float) {
  CAMLparam2(window, points_float);
  CAMLlocal1(tail);
  CAMLlocal1(tuple);
  int total_points = 0;
  tail = points_float;
  while (tail != Val_emptylist) {
    tail = Field(tail, 1);
    ++total_points;
  }
  SDL_FPoint* sdl_points = calloc(total_points, sizeof(SDL_FPoint) * total_points);
  tail = points_float;
  for (int points_idx = 0; points_idx < total_points; ++points_idx) {
    tuple = Field(tail, 0);
    sdl_points[points_idx].x = Double_val(Field(tuple, 0));
    sdl_points[points_idx].y = Double_val(Field(tuple, 1));
    tail = Field(tail, 1);
  }
  SDL_Renderer* renderer = get_renderer_from_window(window);
  int result = SDL_RenderDrawPointsF(renderer, sdl_points, total_points);
  if (result < 0) caml_failwith(SDL_GetError());
  free(sdl_points);
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_render_draw_points(value window, value points) {
  CAMLparam2(window, points);
  CAMLlocal1(tail);
  CAMLlocal1(tuple);
  int total_points = 0;
  tail = points;
  while (tail != Val_emptylist) {
    tail = Field(tail, 1);
    ++total_points;
  }
  SDL_Point* sdl_points = calloc(total_points, sizeof(SDL_Point) * total_points);
  tail = points;
  for (int points_idx = 0; points_idx < total_points; ++points_idx) {
    tuple = Field(tail, 0);
    sdl_points[points_idx].x = Int_val(Field(tuple, 0));
    sdl_points[points_idx].y = Int_val(Field(tuple, 1));
    tail = Field(tail, 1);
  }
  SDL_Renderer* renderer = get_renderer_from_window(window);
  int result = SDL_RenderDrawPoints(renderer, sdl_points, total_points);
  if (result < 0) caml_failwith(SDL_GetError());
  free(sdl_points);
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_render_present(value window) {
  CAMLparam1(window);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  SDL_RenderPresent(renderer);
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_render_clear(value window) {
  CAMLparam1(window);
  SDL_Renderer* renderer = get_renderer_from_window(window);
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

CAMLprim value sdl_set_render_draw_color(value window, int32_t r, int32_t g, int32_t b, int32_t a) {
  CAMLparam1(window);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  int res = SDL_SetRenderDrawColor(renderer, r, g, b, a);
  if (res < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_set_render_draw_color_bytec(value window, value r, value g, value b, value a) {
  CAMLparam5(window, r, g, b, a);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  int res = SDL_SetRenderDrawColor(renderer, Int_val(r), Int_val(g), Int_val(b), Int_val(a));
  if (res < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_renderer_fill_rect_float(value window, value rect) {
  CAMLparam2(window, rect);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  SDL_FRect c_rect = { Double_val(Field(rect, 0)), Double_val(Field(rect, 1)), Double_val(Field(rect, 2)), Double_val(Field(rect, 3)) };
  int res = SDL_RenderFillRectF(renderer, &c_rect);
  if (res < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_renderer_draw_rect_float(value window, value rect) {
  CAMLparam2(window, rect);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  SDL_FRect c_rect = { Double_val(Field(rect, 0)), Double_val(Field(rect, 1)), Double_val(Field(rect, 2)), Double_val(Field(rect, 3)) };
  int res = SDL_RenderDrawRectF(renderer, &c_rect);
  if (res < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_renderer_fill_rect(value window, value rect) {
  CAMLparam2(window, rect);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  SDL_Rect c_rect = { Int_val(Field(rect, 0)), Int_val(Field(rect, 1)), Int_val(Field(rect, 2)), Int_val(Field(rect, 3)) };
  int res = SDL_RenderFillRect(renderer, &c_rect);
  if (res < 0) caml_failwith(SDL_GetError());
  CAMLreturn(Val_unit);
}

CAMLprim value sdl_renderer_draw_rect(value window, value rect) {
  CAMLparam2(window, rect);
  SDL_Renderer* renderer = get_renderer_from_window(window);
  SDL_Rect c_rect = { Int_val(Field(rect, 0)), Int_val(Field(rect, 1)), Int_val(Field(rect, 2)), Int_val(Field(rect, 3)) };
  int res = SDL_RenderDrawRect(renderer, &c_rect);
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
  CAMLlocal1(result);
  int res = SDL_Init(SDL_INIT_VIDEO);
  if (res) {
    result = caml_alloc(1, 1);
    Store_field(result, 0, caml_copy_string(SDL_GetError()));
    CAMLreturn(result);
  }
  result = caml_alloc(1, 0);
  Store_field(result, 0, Val_unit);
  CAMLreturn(result);
}

CAMLprim value sdl_pollevent(value should_wait_val) {
  CAMLparam1(should_wait_val);
  CAMLlocal1(evt_type);
  CAMLlocal1(option_val);
  option_val = Val_none;
  SDL_Event e;
  bool should_wait = Bool_val(should_wait_val);
  int poll_event_value = should_wait ? SDL_WaitEvent(&e) : SDL_PollEvent(&e);
  if (poll_event_value == 1) {
    switch (e.type) {
      case SDL_KEYUP:
      case SDL_KEYDOWN: {
        // tag type is 0 because first variant
        evt_type = caml_alloc(6, 0);
        Store_field(evt_type, 0, Val_int(e.type == SDL_KEYDOWN ? 0 : 1));
        Store_field(evt_type, 1, Val_int(e.key.timestamp));
        Store_field(evt_type, 2, Val_int(e.key.windowID));
        Store_field(evt_type, 3, e.key.state == SDL_PRESSED ? Val_int(0) : Val_int(1));
        Store_field(evt_type, 4, e.key.repeat ? Val_true : Val_false);
        Store_field(evt_type, 5, Val_int(e.key.keysym.sym));
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_MOUSEBUTTONUP:
      case SDL_MOUSEBUTTONDOWN: {
        // tag type is 1 because second variant
        evt_type = caml_alloc(7, 1);
        Store_field(evt_type, 0, Val_int(e.type == SDL_MOUSEBUTTONUP ? 1 : 0));
        Store_field(evt_type, 1, Val_int(e.button.timestamp));
        Store_field(evt_type, 2, Val_int(e.button.windowID));
        Store_field(evt_type, 3, Val_int(e.button.button));
        Store_field(evt_type, 4, Val_int(e.button.clicks));
        Store_field(evt_type, 5, Val_int(e.button.x));
        Store_field(evt_type, 6, Val_int(e.button.y));
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_WINDOWEVENT: {
        int window_evt_type = 4;
        switch (e.window.event) {
          case SDL_WINDOWEVENT_CLOSE: {
            window_evt_type = 0;
          } break;
          case SDL_WINDOWEVENT_RESIZED:
          case SDL_WINDOWEVENT_SIZE_CHANGED: {
            window_evt_type = 1;
          } break;
          case SDL_WINDOWEVENT_FOCUS_GAINED: {
            window_evt_type = 2;
            should_wait = false;
          } break;
          case SDL_WINDOWEVENT_FOCUS_LOST: {
            window_evt_type = 3;
            should_wait = true;
          } break;
          default: {}
        }
          // tag type is 2 because third variant
          evt_type = caml_alloc(3, 2);
          Store_field(evt_type, 0, Val_int(e.window.timestamp));
          Store_field(evt_type, 1, Val_int(e.window.windowID));
          Store_field(evt_type, 2, Val_int(window_evt_type));
          option_val = caml_alloc_some(evt_type);
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
      } break;
      case SDL_QUIT: {
        evt_type = Val_int(0);
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_TEXTINPUT: {
        evt_type = caml_alloc(3, 4);
        Store_field(evt_type, 0, Val_int(e.text.timestamp));
        Store_field(evt_type, 1, Val_int(e.text.windowID));
        Store_field(evt_type, 2, caml_copy_string(e.text.text));
        option_val = caml_alloc_some(evt_type);
      } break;
      case SDL_MOUSEWHEEL: {
        evt_type = caml_alloc(4, 5);
        Store_field(evt_type, 0, Val_int(e.wheel.mouseX));
        Store_field(evt_type, 1, Val_int(e.wheel.mouseY));
        Store_field(evt_type, 2, Val_int(e.wheel.x));
        Store_field(evt_type, 3, Val_int(e.wheel.y));
        option_val = caml_alloc_some(evt_type);
      } break;
    }
  }
  CAMLreturn(option_val);
}
