# limitless - text editor written in ocaml bc fuck it

todo:

look into freetype and harfbuzz
think about statically linking things vs dynamically linking things

gotta handle modifier key + key somehow...either through sdl_textinputevent or i just have to manually implement the logic for it
 - I'm using sdl_textinputevent to handle keys resulting from modifiers and then using sdl_keydown events to handle backspaces etc
implement rope data structure
think about text cursors

gotta write opengl bindings/stub code. realized that software rendering individual pixels is slow without gpu parallelism

gotta rewrite some of the stub code that errors with caml_failwith to use result types so that it's clear in the ocaml code that
an error can occur and the error needs to be handled. using functions that only do side effects and dont tell the caller anything is
messy.
