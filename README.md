# tack - text editor written in ocaml bc fuck it

todo:

look into freetype and harfbuzz
think about statically linking things vs dynamically linking things

gotta handle modifier key + key somehow...either through sdl_textinputevent or i just have to manually implement the logic for it
 - I'm using sdl_textinputevent to handle keys resulting from modifiers and then using sdl_keydown events to handle backspaces etc
think about text cursors

gotta rewrite some of the stub code that errors with caml_failwith to use result types so that it's clear in the ocaml code that
an error can occur and the error needs to be handled. using functions that only do side effects and dont tell the caller anything is
messy.

maybe use doxygen and ocamldoc for documentation generation.

UI could potentially be simplified to an interface where everything is a "Box" and that box just resizes with it's parent container
and contains text

vim motions?

basic features:
  basic text manipulation like highlighting, deleting, copy/paste
  multi cursors
  LSP
  text search

current questions:
  how to set proper point size if DPI isnt easily accessible?
current problems:
  memory usage is too high
  performance is not ideal (still need to test on large amounts of text)
  text wrapping is dogshit
    - write_to_buffer might have to return the x_offset value due to adjustments when wrapping
      so that wrapped lines that occur after are consistent. (The adjusted values aren't kept so glyphs that are positioned after the wrapped line, behave as thought they didn't occur after a adjusted value).

thoughts about cursor related problems:
    abstract away the logic for offsetting the x_offset so that it's easily known what glyph a cursor lands on when moving around.
    a lot of logic will surround the rope data structure and traversing it for information (like what glyph the cursor lands on).

maybe there should be colors (RGB) as well in our vertex attrib arrays
