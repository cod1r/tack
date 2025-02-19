open Limitless.Events;;
open Limitless.Sdl;;

let () = init_sdl ();;

let w = sdl_create_window "limitless" 0 0 800 800 (sdl_window_resizable lor sdl_window_opengl);;

match w with
| Some(Window { width; height; title }) ->
    Printf.printf "Created window: %s %d %d" title width height; print_newline ()
| None -> failwith "unable to create window";;

let rec loop () =
  let evt = sdl_pollevent () in
  let continue = match evt with
    | Some(KeyboardEvt { keysym; timestamp; }) ->
        Printf.printf "KBD: %c, %Ld" keysym timestamp;
        print_newline ();
        if keysym = 'q' then false else true
    | Some(MouseButtonEvt {
        mouse_evt_type;
        timestamp;
        x;
        y;
        windowID;
        button;
        clicks;
      }) ->
        begin
          match mouse_evt_type with
          | Mousedown ->
              Printf.printf "Mousedown %Ld, %Ld, %Ld, %ld, %ld, %Ld" x y windowID button clicks timestamp; print_newline ();
          | Mouseup -> Printf.printf "Mouseup"; print_newline();
        end; true
    | Some(WindowEvt { event }) ->
        begin
          match event with
          | WindowClose -> false
          | WindowResize -> true
        end
    | None -> true in
    if continue then loop () else ();;

let _ = loop ();;
