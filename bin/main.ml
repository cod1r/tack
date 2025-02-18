open Limitless.Events;;
open Limitless.Sdl;;

let () = init_sdl ();;

let rec loop () =
  let evt = sdl_pollevent () in
  let continue = match evt with
    | Some(KeyboardEvt { keysym=keysym; timestamp=timestamp; }) ->
        Printf.printf "KBD: %c, %Ld" keysym timestamp;
        print_newline ();
        if keysym = 'q' then false else true
    | Some(MouseButtonEvt {
        mouse_evt_type=mouse_evt_type;
        timestamp=timestamp;
        x=x;
        y=y;
        windowID=windowID;
        button=button;
        clicks=clicks;
      }) ->
        begin
          match mouse_evt_type with
          | Mousedown -> Printf.printf "Mousedown %Ld, %Ld, %Ld, %ld, %ld, %Ld" x y windowID button clicks timestamp; print_newline ();
          | Mouseup -> Printf.printf "Mouseup"; print_newline();
        end; true
    | Some(WindowEvt { event=event }) ->
        begin
          match event with
          | WindowClose -> false
          | WindowResize -> true
        end
    | None -> true in
    if continue then loop () else ();;

let _ = loop ();;
