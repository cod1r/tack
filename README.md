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

    core idea for now is that "Box"s will can be manually sized or automatically laid out by some runtime layout logic
    Hierarchy will be specified with 'content' property

    borders?

    border radius?
        - ellipse formula (x^2/a^2 + y^2/b^2 = 1)
        - each corner of the box is curved using an ellipse

    need to separate rendering of the box structure and the application of ui rules such as width, height calculation...

		to make rendering a bit faster, i need to keep the minimum and max rope position that indicates the interval of text
		that is going to be rendered on the screen. a new gadt variant can be introduced to change how traverse_rope works.

		need to consolidate ui lib rules to make sense of how the primitive layout/sizing pieces come together

		having multiple places that call traverse_rope to calculate placement is too fragile
		  need to provide better interface for traversing the rope and accumulating values

    set width and height as percentages relative to the parent size.

    refactor box_content to be GADTs

    currently there's an issue where the child box will try and max its size to the parents size and the parent will try and min
      its size to the child's size so neither gets the right size...

    UI layout features:
      vertical/horizontal "flow" layout
      vertical/horizontal/center alignment

    size min/max'ing needs to be rethought...

    a lot of logic is fragile and needs to be tested

performance ideas:
	- the main issue is with edits to the rope. rope changes cause recalculations to occur which means the function traverse_rope
		gets called a lot. If the file that's being edited, has 1 billion characters (1 gigabyte), each edit or interaction would
		take forever. There needs to be a way to do edits and recalculate information (such as wrapping, closest cursor position)
		without iterating over all characters.
	- first priority should be to make sure cursor operations are fast.

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
  - There is too much nested state, so things are hard to read. this means another session of code cleanup
  - every file should keep track of its scroll position
  - every file needs to occasionally poll if it's contents have changed on disk and make a decision about current changes vs
    changes on disk
  - build some sort of a layout "engine"
  - make some ui elements that can handle text behaviors

findings about behavior on linux vs macos:
  - on linux, resize events are sent every time a slight change in size happens whereas on macos, the resize event is sent after the resizing is done
  - on linux, the opengl viewport is not resized to match the window size/resize but on macos, it is

Current design thoughts:
- The "editor" is a ui component that has line numbers and an area to display text.
It will be used to display anything that can be displayed with text; eg - file searching, etc
- Maybe things should be built like a window manager but for text editing?
- Binary Space Partition?
