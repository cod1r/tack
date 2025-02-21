type keyboardEvtType = Keydown | Keyup
type keyboardEvtState = Pressed | Released
type mouseEvtType = Mousedown | Mouseup
type mouseEvtState = Pressed | Released
type windowEvtType = WindowClose | WindowResize
type event = KeyboardEvt of {
  kbd_evt_type: keyboardEvtType;
  timestamp: int;
  windowID: int;
  state: keyboardEvtState;
  repeat: bool;
  keysym: char;
} | MouseButtonEvt of {
  mouse_evt_type: mouseEvtType;
  timestamp: int;
  windowID: int;
  button: int;
  clicks: int;
  x: int;
  y: int;
} | WindowEvt of {
  timestamp: int;
  windowID: int;
  event: windowEvtType;
} | MouseMotionEvt of {
  timestamp: int;
  windowID: int;
  which: int;
  x: int;
  y: int;
  xrel: int;
  yrel: int;
};;
