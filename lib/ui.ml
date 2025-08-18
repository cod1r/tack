type bounding_box = { width : int; height : int; x : int; y : int }
type direction = Horizontal | Vertical

type box = {
  content : box_content;
  bbox : bounding_box;
  text_wrap : bool;
  border : bool;
  flow : direction;
  take_remaining_space : direction option;
}

and box_content = Box of box | Text of string
