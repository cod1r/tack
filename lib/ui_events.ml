let event_handlers = ref []

let emit_event ~(e : Sdl.Sdl.event) =
  List.iter (fun event_handler -> event_handler ~e) !event_handlers
