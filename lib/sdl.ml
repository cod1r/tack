module Sdl = struct
  type keyboardEvtType = Keydown | Keyup
  type keyboardEvtState = Pressed | Released
  type mouseEvtType = Mousedown | Mouseup
  type mouseEvtState = Pressed | Released
  type windowEvtType = WindowClose | WindowResize

  type event =
    | KeyboardEvt of {
        kbd_evt_type : keyboardEvtType;
        timestamp : int;
        windowID : int;
        state : keyboardEvtState;
        repeat : bool;
        keysym : char;
      }
    | MouseButtonEvt of {
        mouse_evt_type : mouseEvtType;
        timestamp : int;
        windowID : int;
        button : int;
        clicks : int;
        x : int;
        y : int;
      }
    | WindowEvt of { timestamp : int; windowID : int; event : windowEvtType }
    | MouseMotionEvt of {
        timestamp : int;
        windowID : int;
        which : int;
        x : int;
        y : int;
        xrel : int;
        yrel : int;
      }
    | TextInputEvt of { timestamp : int; windowID : int; text : string }
    | Quit

  external sdl_delay : int -> unit = "SDL_Delay" "SDL_Delay"
  external init_sdl : unit -> unit = "init_sdl" "init_sdl"

  external sdl_pollevent : unit -> event option
    = "sdl_pollevent" "sdl_pollevent"

  type window =
    | Window of {
        id : int;
        title : string;
        x : int;
        y : int;
        width : int;
        height : int;
      }

  type window_size = int * int

  type rect =
    | Rect of { x : int; y : int; width : int; height : int }
    | RectF of { x : float; y : float; width : float; height : float }

  type point = Point of int * int
  type pointf = PointF of float * float

  external sdl_renderer_fill_rect_float : window -> rect -> unit
    = "sdl_renderer_fill_rect_float" "sdl_renderer_fill_rect_float"

  external sdl_renderer_draw_rect_float : window -> rect -> unit
    = "sdl_renderer_draw_rect_float" "sdl_renderer_draw_rect_float"

  external sdl_get_renderer_size : window -> int * int
    = "sdl_get_renderer_size" "sdl_get_renderer_size"

  external sdl_get_window_size : window -> window_size
    = "sdl_get_window_size" "sdl_get_window_size"

  external sdl_set_render_draw_blendmode : window -> int -> unit
    = "sdl_set_render_draw_blendmode" "sdl_set_render_draw_blendmode"

  external sdl_render_draw_point_f : window -> float -> float -> unit
    = "sdl_render_draw_point_f" "sdl_render_draw_point_f"

  external sdl_render_draw_points_float : window -> pointf list -> unit
    = "sdl_render_draw_points_float" "sdl_render_draw_points_float"

  external sdl_render_draw_points : window -> point list -> unit
    = "sdl_render_draw_points" "sdl_render_draw_points"

  external sdl_create_window :
    string -> int -> int -> int -> int -> int -> window option
    = "sdl_create_window" "sdl_create_window"

  external sdl_render_present : window -> unit
    = "sdl_render_present" "sdl_render_present"

  external sdl_render_clear : window -> unit
    = "sdl_render_clear" "sdl_render_clear"

  external sdl_renderer_fill_rect : window -> rect -> unit
    = "sdl_renderer_fill_rect" "sdl_renderer_fill_rect"

  external sdl_renderer_draw_rect : window -> rect -> unit
    = "sdl_renderer_draw_rect" "sdl_renderer_draw_rect"

  external sdl_set_render_draw_color :
    window -> int -> int -> int -> int -> unit
    = "sdl_set_render_draw_color" "sdl_set_render_draw_color"

  external sdl_create_renderer : window -> int -> unit
    = "sdl_create_renderer" "sdl_create_renderer"
end

let sdl_window_resizable = 0x00000020
let sdl_window_opengl = 0x00000002
let sdl_window_shown = 0x00000004
let sdl_window_allow_highdpi = 0x00002000
let sdl_blendmode_blend = 0x00000001
let sdl_renderer_software = 0x00000001
let sdl_renderer_accelerated = 0x00000002
