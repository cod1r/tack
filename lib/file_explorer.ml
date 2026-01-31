open Sdl

type file_explorer = { mutable opened_directory : Files.file_tree option }

let file_explorer = { opened_directory = None }
