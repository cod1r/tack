let event_handlers = ref []

let emit_event ~(e : Sdl.Sdl.event) =
  List.iter (fun event_handler -> event_handler ~e) !event_handlers

let add_event_handler ~(event_handler : e:Sdl.Sdl.event -> unit) =
  event_handlers := event_handler :: !event_handlers
