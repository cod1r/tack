module Sdl = struct
  type keyboardEvtType = Keydown | Keyup
  type keyboardEvtState = Pressed | Released
  type mouseEvtType = Mousedown | Mouseup
  type mouseEvtState = Pressed | Released
  type windowEvtType = WindowClose | WindowResize | Unhandled

  let sdl_window_resizable = 0x00000020
  let sdl_window_opengl = 0x00000002
  let sdl_window_shown = 0x00000004
  let sdl_window_allow_highdpi = 0x00002000
  let sdl_blendmode_blend = 0x00000001
  let sdl_renderer_software = 0x00000001
  let sdl_renderer_accelerated = 0x00000002

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
    | MouseWheelEvt of { preciseX : float; preciseY : float; x : int; y : int }

  external sdl_delay : int -> unit = "SDL_Delay" "SDL_Delay"
  external init_sdl : unit -> (unit, string) result = "init_sdl" "init_sdl"

  external sdl_waitevent : (unit[@untagged]) -> event option
    = "sdl_waitevent" "sdl_waitevent"

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

  external set_clipboard_text : string -> unit
    = "set_clipboard_text" "set_clipboard_text"

  external get_clipboard_text : unit -> string
    = "get_clipboard_text" "get_clipboard_text"

  external sdl_gl_swapwindow : window -> unit
    = "sdl_gl_swapwindow" [@@noalloc]

  external sdl_gl_make_current : window -> (unit, string) result
    = "sdl_gl_make_current" "sdl_gl_make_current"

  external sdl_gl_create_context : window -> (unit, string) result
    = "sdl_gl_create_context" "sdl_gl_create_context"

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

  external sdl_render_draw_point_f :
    window -> (float[@unboxed]) -> (float[@unboxed]) -> unit
    = "sdl_render_draw_point_f_bytec" "sdl_render_draw_point_f"

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
    window ->
    (int32[@unboxed]) ->
    (int32[@unboxed]) ->
    (int32[@unboxed]) ->
    (int32[@unboxed]) ->
    unit = "sdl_set_render_draw_color_bytec" "sdl_set_render_draw_color"

  external sdl_create_renderer : window -> int -> unit
    = "sdl_create_renderer" "sdl_create_renderer"

  external sdl_gl_getdrawablesize : (unit[@untagged]) -> int * int
    = "sdl_gl_getdrawablesize" "sdl_gl_getdrawablesize"

  external sdl_create_and_set_system_cursor : unit -> unit
    = "sdl_create_and_set_system_cursor" "sdl_create_and_set_system_cursor"

  external sdl_gl_getswapinterval : unit -> int = "sdl_gl_getswapinterval" [@@noalloc]
  external sdl_gl_setswapinterval : int -> unit = "sdl_gl_setswapinterval" [@@noalloc]

  let actually_init_sdl () =
    (match init_sdl () with Ok () -> () | Error e -> failwith e);
    let w =
      match
        sdl_create_window "tack" 0 0 1000 1000
          (sdl_window_allow_highdpi lor sdl_window_opengl
         lor sdl_window_resizable)
      with
      | Some (Window { width; height; title; _ } as w) ->
          Printf.printf "Created window: %s %d %d" title width height;
          print_newline ();
          w
      | None -> failwith "unable to create window"
    in
    (match sdl_gl_create_context w with Ok () -> () | Error e -> failwith e);
    (match sdl_gl_make_current w with Ok () -> () | Error e -> failwith e);
    w

  let w = actually_init_sdl ()
  let () = Opengl.glew_init ()
end
