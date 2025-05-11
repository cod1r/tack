open OUnit2
open Tack.Freetype
open Tack.Sdl
open Tack.Render

let timing_test_opengl_works _ =
  let w =
    match Sdl.sdl_create_window "tack" 0 0 800 800 Sdl.sdl_window_opengl with
    | Some (Window { width; height; title; _ } as w) ->
        Printf.printf "Created window: %s %d %d" title width height;
        print_newline ();
        w
    | None -> failwith "unable to create window"
  in
  let _ =
    match Sdl.sdl_gl_create_context w with Ok () -> () | Error e -> failwith e
  in
  let _ =
    match Sdl.sdl_gl_make_current w with Ok () -> () | Error e -> failwith e
  in
  let _ = Sdl.sdl_waitevent () in
  for _ = 0 to 100_000 do
    Tack.Opengl.gl_clear_color 1. 0. 0. 1.;
    Tack.Opengl.gl_clear ();
    (match Sdl.sdl_gl_swapwindow w with Ok () -> () | Error e -> failwith e);
    Tack.Opengl.gl_clear_color 0. 1. 0. 1.;
    Tack.Opengl.gl_clear ();
    (match Sdl.sdl_gl_swapwindow w with Ok () -> () | Error e -> failwith e);
    Tack.Opengl.gl_clear_color 0. 0. 1. 1.;
    Tack.Opengl.gl_clear ();
    match Sdl.sdl_gl_swapwindow w with Ok () -> () | Error e -> failwith e
  done

let gl_gen_one_buffer_test _ =
  let buffer = Tack.Opengl.gl_gen_one_buffer () in
  assert_bool "buffer should not equal 0 or be less than 0" (buffer != 0)

let timing_test_writing_bigarray _ =
  let start = Unix.gettimeofday () in
  let bigarray = Bigarray.Array1.create Float32 C_layout 10_000_000 in
  for i = 0 to Bigarray.Array1.dim bigarray - 1 do
    bigarray.{i} <- i |> Int.to_float
  done;
  let end' = Unix.gettimeofday () -. start in
  assert_bool
    ("Writing "
    ^ Int.to_string (Bigarray.Array1.dim bigarray)
    ^ "i times in big array slow")
    (end' < 0.09)

let tests =
  "render tests"
  >::: [ (*"gl set up" >:: timing_test_opengl_works;*)
         (* "gl_gen_one_buffer test" >:: gl_gen_one_buffer_test; *)
         (* "writing to bigarray time test" >:: timing_test_writing_bigarray; *) ]

let () = run_test_tt_main tests
