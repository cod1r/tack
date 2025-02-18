external sdl_delay: int -> unit = "SDL_Delay" "SDL_Delay"
external init_sdl: unit -> unit = "init_sdl" "init_sdl"
type sdl_event = {
  evt_type: int64
}[@@boxed];;
external sdl_pollevent: sdl_event -> unit = "sdl_pollevent" "sdl_pollevent"
let evt = { evt_type = 0L };;
let () = init_sdl ();;
let () =
  sdl_pollevent evt;
  let evt_val = match evt with
  | { evt_type = evt_type } -> evt_type in
  Printf.printf "%LX\n" evt_val;
  sdl_delay 2000;;
