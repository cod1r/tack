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

type dimension =
  | Width
  | Height

type size_constraint =
  (* clamp to content size *)
  | Content of { fallback_size : int }
  (* clamp to parent size *)
  | Parent of { fallback_size : int }
  (* clamp to content size but there is a minimum/maximum constraint *)
  | MinMaxContent of
      { min : int
      ; max : int
      }
  (* clamp to parent size but there is a minimum/maximum constraint *)
  | MinMaxParent of
      { min : int
      ; max : int
      }
  (* clamp to content size but there is a maximum constraint *)
  | MaxContent of { max : int }
  (* clamp to content size but there is a minimum constraint *)
  | MinContent of { min : int }
  (* clamp to parent size but there is a maximum constraint *)
  | MaxParent of { max : int }
  (* clamp to parent size but there is a minimum constraint *)
  | MinParent of { min : int }
  (* fixed number *)
  | Number of int
  (* in the case of child expanding to parent size and parent wanting to clamp to child size
 there needs to be fallback *)
  | ExpandAsMuchPossible of { fallback_size : int }

type corner_options =
  { vertical_radius : int
  ; horizontal_radius : int
  }

type border_options =
  { top_thickness : int
  ; right_thickness : int
  ; left_thickness : int
  ; bottom_thickness : int
  ; top_left_corner_options : corner_options
  ; top_right_corner_options : corner_options
  ; bottom_left_corner_options : corner_options
  ; bottom_right_corner_options : corner_options
  ; color : float * float * float * float
  }

type box =
  { mutable name : string option
  ; mutable update : (unit -> unit) option
  ; mutable content : box_content option
    (* im thinking that bbox should be something that the user never writes to directly *)
  ; mutable bbox : bounding_box option
  ; mutable text_wrap : bool
  ; mutable background_color : float * float * float * float
  ; mutable border : border_options option
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
