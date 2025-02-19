open Events;;
external sdl_delay: int -> unit = "SDL_Delay" "SDL_Delay"
external init_sdl: unit -> unit = "init_sdl" "init_sdl"
external sdl_pollevent: unit -> event option = "sdl_pollevent" "sdl_pollevent"
type window = Window of {
  title: string;
  x: int;
  y: int;
  width: int;
  height: int;
};;
external sdl_create_window: string -> int -> int -> int -> int -> int -> window option = "sdl_create_window" "sdl_create_window"
let sdl_window_resizable = 0x00000020;;
let sdl_window_opengl = 0x00000002;;
let sdl_window_shown = 0x00000004;;
