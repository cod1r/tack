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

thoughts about cursor related problems:
    abstract away the logic for offsetting the x_offset so that it's easily known what glyph a cursor lands on when moving around.
    a lot of logic will surround the rope data structure and traversing it for information (like what glyph the cursor lands on).

maybe there should be colors (RGB) as well in our vertex attrib arrays

Using Int_val instead of Long_val might fuck me over in the future. idk...

there needs to be a faster and better way to go from screen coords to a position in the rope data structure.

CLEAN UP CODE!!

displaying line numbers

Things to note and remember:
  text is positioned by taking it's relative horizontal position in the rope and wrapping the value around the window width.

for implementing highlighting:
  - use GL_QUAD_STRIPS
  - text color might need to be considered if alpha channel doesn't allow text to be easily seen through the highlight
  - use the same buffer for text; just include RGB so now each point has 6 components - x,y,r,g,b,a
  - might need to pass in a boolean for when to draw blue for vertices that aren't used to draw characters (white/black pixels)
