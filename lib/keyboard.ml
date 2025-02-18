type keyboardEvtType = Keydown | Keyup
type keyboardEvtState = Pressed | Released
type keyboardEvt = KeyboardEvt of {
  kbd_evt_type: keyboardEvtType;
  timestamp: int64;
  windowID: int64;
  state: keyboardEvtState;
  repeat: bool;
  keysym: char;
};;
