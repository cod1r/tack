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

Using Int_val instead of Long_val might fuck me over in the future. idk...

there needs to be a faster and better way to go from screen coords to a position in the rope data structure.

CLEAN UP CODE!!

displaying line numbers

Things to note and remember:
  text is positioned by taking it's relative horizontal position in the rope and wrapping the value around the window width.

blinking cursor?

arrow keys should properly move text caret

UNDO TREE SHIT

SOME WAY TO SWITCH FILES QUICKLY

MOUSEDOWN handling needs to be more accurate

rope data structure needs to be built in a way such that there is less depth for SPEED (basically balance between leaf length and tree depth)

more things to do:
  - LINE NUMBERS NEED TO BE IMPLEMENTED
  - for some reason, when switching to a file, text is highlighted from the beginning to where the last cursor click position was
  - There is too much nested state, so things are hard to read. this means another session of code cleanup
  - every file should keep track of its scroll position
  - every file needs to occasionally poll if it's contents have changed on disk and make a decision about current changes vs
    changes on disk
  - build some sort of a layout "engine"
  - make some ui elements that can handle text behaviors

findings about behavior on linux vs macos:
  - on linux, resize events are sent every time a slight change in size happens whereas on macos, the resize event is sent after the resizing is done
  - on linux, the opengl viewport is not resized to match the window size/resize but on macos, it is
