open Limitless.Events;;
external sdl_delay: int -> unit = "SDL_Delay" "SDL_Delay"
external init_sdl: unit -> unit = "init_sdl" "init_sdl"
external sdl_pollevent: unit -> event option = "sdl_pollevent" "sdl_pollevent"

let () = init_sdl ();;

let rec loop () =
  let evt = sdl_pollevent () in
  let continue = match evt with
    | Some(KeyboardEvt { keysym=keysym; _ }) -> Printf.printf "KBD: %c" keysym; print_newline (); if keysym = 'q' then false else true
    | Some(MouseButtonEvt { mouse_evt_type=mouse_evt_type; }) -> Printf.printf "MOUSE"; print_newline (); true
    | None -> true in
    if continue then loop () else ();;

let _ = loop ();;
