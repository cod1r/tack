open Limitless.Events;;
external sdl_delay: int -> unit = "SDL_Delay" "SDL_Delay"
external init_sdl: unit -> unit = "init_sdl" "init_sdl"
external sdl_pollevent: unit -> event option = "sdl_pollevent" "sdl_pollevent"

let () = init_sdl ();;

let rec loop () =
  let evt = sdl_pollevent () in
  begin
    match evt with
    | Some(KeyboardEvt { keysym=keysym; _ }) -> Printf.printf "KBD: %c" keysym; print_newline ()
    | Some(MouseButtonEvt { windowID=windowID; }) -> Printf.printf "MOUSE: %Ld" windowID; print_newline ()
    | None -> ()
  end;
  loop ();;

let _ = loop ();;
