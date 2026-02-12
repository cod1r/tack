type bounding_box =
  { mutable width : int
  ; mutable height : int
  ; mutable x : int
  ; mutable y : int
  }

type direction =
  | Horizontal
  | Vertical

type box_sides =
  { left : int
  ; right : int
  ; top : int
  ; bottom : int
  }

type positioning =
  | Relative of
      { x : int
      ; y : int
      }
  | Absolute

type horizontal_alignment =
  | Left
  | Center
  | Right

type vertical_alignment =
  | Top
  | Center
  | Bottom

type size_constraint_type =
  | Min
  | Max

type size_constraint =
  { constraint_type : size_constraint_type
  ; fallback_size : int
  }

type box =
  { mutable name : string option
  ; mutable update : (unit -> unit) option
  ; mutable content : box_content option
  ; mutable bbox : bounding_box option
  ; mutable text_wrap : bool
  ; mutable background_color : float * float * float * float
  ; mutable border : bool
  ; mutable flow : direction option
  ; mutable font_size : int option
  ; mutable width_constraint : size_constraint option
  ; mutable height_constraint : size_constraint option
  ; mutable clip_content : bool
  ; mutable position_type : positioning
  ; mutable allow_horizontal_scroll : bool
  ; mutable allow_vertical_scroll : bool
  ; mutable horizontal_align : horizontal_alignment option
  ; mutable vertical_align : vertical_alignment option
  ; mutable on_event : event_handler_t option
  ; mutable scroll_x_offset : int
  ; mutable scroll_y_offset : int
  ; mutable focusable : bool
  }

and event_handler_t = b:box option -> e:Sdl.event -> unit

and string_action_info =
  { string : string
  ; pos : int
  }

and action =
  | Insertion of string_action_info
  | Deletion of string_action_info

and history_entries =
  { undo_list : action list
  ; redo_list : action list
  }

and text_area_information =
  { text : Rope_types.rope option
  ; cursor_pos : int option
  ; highlight_pos : int option * int option
  ; holding_mousedown_rope_pos : int option
  ; history : history_entries
  }

and vertical_scroll_info =
  { vertical_scroll : box
  ; vertical_scrollbar_container : box
  }

and horizontal_scroll_info =
  { horizontal_scroll : box
  ; horizontal_scrollbar_container : box
  }

and scrollcontainer_info =
  { vertical_scroll_info : vertical_scroll_info option
  ; horizontal_scroll_info : horizontal_scroll_info option
  }

and box_content =
  | Box of box
  | Boxes of box list
  | Text of { string : string }
  | Textarea of text_area_information

type ui_traversal_context =
  { in_scrollcontainer : bool
  ; parent : box option
  }
