open Events;;
external sdl_delay: int -> unit = "SDL_Delay" "SDL_Delay"
external init_sdl: unit -> unit = "init_sdl" "init_sdl"
external sdl_pollevent: unit -> event option = "sdl_pollevent" "sdl_pollevent"
