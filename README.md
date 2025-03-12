# limitless - text editor written in ocaml bc fuck it

todo:

decide whether to software render or gpu render things
 - probably going to software render
look into freetype and harfbuzz
think about statically linking things vs dynamically linking things


gotta handle modifier key + key somehow...either through sdl_textinputevent or i just have to manually implement the logic for it
 - I'm using sdl_textinputevent to handle keys resulting from modifiers and then using sdl_keydown events to handle backspaces etc
implement rope data structure
think about text cursors

think about sdl_surface and sdl_texture with how those structures can help with redrawing the screen and having correct
pixels; how will sdl_surface and sdl_texture be represented in ocaml - should abstract datatypes start being used, etc
