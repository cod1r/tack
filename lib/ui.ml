type bounding_box = { width : int; height : int; x : int; y : int }
type direction = Horizontal | Vertical

type box = {
  content : box_content;
  bbox : bounding_box;
  text_wrap : bool;
  background_color : string;
  border : bool;
  flow : direction;
  take_remaining_space : direction option;
}

and box_content = Box of box | Boxes of box list | Text of string

(*
 core idea for now is that "Box"s will mainly bc manually sized
 and children "Box"s will be able to fill in remaining space or also be manually sized
 "Box"s will also be able to flow in the horizontal or vertical direction
 Hierarchy will be specified with 'content' property
 *)
