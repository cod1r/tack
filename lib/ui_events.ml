let event_handlers = ref []

let emit_event ~(e : Sdl.Sdl.event) =
  List.iter
    (fun (box, event_handler) -> event_handler ~b:box ~e)
    !event_handlers

let add_event_handler ~(box : Ui.box option)
    ~(event_handler : Ui.event_handler_t) =
  event_handlers := (box, event_handler) :: !event_handlers
