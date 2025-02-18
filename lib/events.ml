type keyboardEvtType = Keydown | Keyup
type keyboardEvtState = Pressed | Released
type mouseEvtType = Mousedown | Mouseup
type mouseEvtState = Pressed | Released
type event = KeyboardEvt of {
  kbd_evt_type: keyboardEvtType;
  timestamp: int64;
  windowID: int64;
  state: keyboardEvtState;
  repeat: bool;
  keysym: char;
} | MouseButtonEvt of {
  mouse_evt_type: mouseEvtType;
  timestamp: int64;
  windowID: int64;
  button: int32;
  clicks: int32;
  x: int64;
  y: int64;
};;
