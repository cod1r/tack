type rope = Leaf of string | Node of {left: rope; right: rope; weight: int}

type rope_traversal_info = {x: int; y: int; rope_pos: int}

type pos = Y of int | X of int

type closest_information =
  { closest_col: int option
  ; upper_y: int
  ; closest_vertical_range: (int * int) option
  ; closest_rope: int option
  ; original_pos: pos }

type line_number_info = rope_traversal_info * int option list

type find_cursor_info = rope_traversal_info * closest_information

type _ traverse_info =
  | Rope_Traversal_Info :
      rope_traversal_info
      -> rope_traversal_info traverse_info
  | Line_Numbers : line_number_info -> line_number_info traverse_info
  | Finding_Cursor : find_cursor_info -> find_cursor_info traverse_info
