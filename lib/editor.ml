open Freetype
open Rope

module Editor = struct
  type editor = { rope : Rope.rope option; cursor_pos : int * int }
end
