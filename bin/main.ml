open Limitless.Events;;
external sdl_delay: int -> unit = "SDL_Delay" "SDL_Delay"
external init_sdl: unit -> unit = "init_sdl" "init_sdl"
external sdl_pollevent: unit -> event option = "sdl_pollevent" "sdl_pollevent"

let () = init_sdl ();;

let rec loop () =
  let evt = sdl_pollevent () in
  let continue = match evt with
    | Some(KeyboardEvt { keysym=keysym; timestamp=timestamp; }) -> Printf.printf "KBD: %c, %Ld" keysym timestamp; print_newline (); if keysym = 'q' then false else true
    | Some(MouseButtonEvt { mouse_evt_type=mouse_evt_type; timestamp=timestamp; x=x; y=y; }) ->
        begin
          match mouse_evt_type with
          | Mousedown -> Printf.printf "Mousedown %Ld, %Ld" x y; print_newline ();
          | Mouseup -> Printf.printf "Mouseup"; print_newline();
        end; true
    | None -> true in
    if continue then loop () else ();;

let _ = loop ();;
