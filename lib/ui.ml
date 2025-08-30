type bounding_box = { width : int; height : int; x : int; y : int }
type direction = Horizontal | Vertical | Both
type box_sides = { left : int; right : int; top : int; bottom : int }

type box = {
  content : box_content option;
  bbox : bounding_box;
  text_wrap : bool;
  background_color : float * float * float * float;
  border : bool;
  flow : direction option;
  take_remaining_space : direction option;
}

and box_content = Box of box | Boxes of box list | Text of string
