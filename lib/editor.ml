(* this file should contain types and logic relating to the internals of the editor *)
type editor = {
 buffer : string;
 rope: Rope.rope;
 cursor_pos : int * int 
}
