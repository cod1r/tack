open Events;;
external sdl_delay: int -> unit = "SDL_Delay" "SDL_Delay"
external init_sdl: unit -> unit = "init_sdl" "init_sdl"
external sdl_pollevent: unit -> event option = "sdl_pollevent" "sdl_pollevent"
type window = Window of {
  id: int;
  title: string;
  x: int;
  y: int;
  width: int;
  height: int;
};;

type rect = Rect of {
  x: int;
  y: int;
  width: int;
  height: int;
};;

external sdl_create_window: string -> int -> int -> int -> int -> int -> window option = "sdl_create_window" "sdl_create_window"
external sdl_render_present: window -> unit = "sdl_render_present" "sdl_render_present"
external sdl_render_clear: window -> unit = "sdl_render_clear" "sdl_render_clear"
external sdl_renderer_fill_rect: window -> rect -> unit = "sdl_renderer_fill_rect" "sdl_renderer_fill_rect"
external sdl_renderer_draw_rect: window -> rect -> unit = "sdl_renderer_draw_rect" "sdl_renderer_draw_rect"
external sdl_set_render_draw_color: window -> int -> int -> int -> int -> unit = "sdl_set_render_draw_color" "sdl_set_render_draw_color"
external sdl_create_renderer: window -> int -> unit = "sdl_create_renderer" "sdl_create_renderer"
let sdl_window_resizable = 0x00000020;;
let sdl_window_opengl = 0x00000002;;
let sdl_window_shown = 0x00000004;;
