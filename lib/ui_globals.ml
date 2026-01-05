open Ui_types

let focused_element : box option ref = ref None
let scrollbar_container_width = 15
let set_focused_element ~(box : box) = focused_element := Some box
let unfocus_element () = focused_element := None
let scrollcontainers : (box * scrollcontainer_info) list ref = ref []

let holding_mousedown : [ `True of original_x:int * original_y:int | `False ] ref =
  ref `False
;;

let holding_ctrl = ref false
