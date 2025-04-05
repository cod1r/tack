open Freetype
open Rope

module Editor = struct
  type cursor_pos_tuple = int * int
  type editor = { rope : Rope.rope option; cursor_pos : cursor_pos_tuple }
end
