open Limitless.Keyboard;;
external sdl_delay: int -> unit = "SDL_Delay" "SDL_Delay"
external init_sdl: unit -> unit = "init_sdl" "init_sdl"
external sdl_pollevent: unit -> keyboardEvt option = "sdl_pollevent" "sdl_pollevent"
let () = init_sdl ();;
let rec loop () =
  let evt = sdl_pollevent () in
  match evt with
  | Some(e) ->
      begin
        match e with
        | KeyboardEvt { keysym=keysym; _ } -> Printf.printf "KEYSYM: %c" keysym; print_newline ();
      end
  | None -> ();
  loop ();;
let _ = loop ();;
