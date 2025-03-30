open OUnit2
open Limitless.Freetype
open Limitless.Sdl
open Limitless.Render

let () = FreeType.freetype_init ()
let () = FreeType.freetype_load_font ()
let () = FreeType.freetype_set_pixel_sizes 6

let glyph_infos =
  let startcode, endcode = (32, 126) in
  let rec get_glyph_info char_code acc =
    if char_code > endcode then acc
    else
      let new_glyph_info =
        FreeType.freetype_load_glyph_letter (Char.chr char_code)
      in
      get_glyph_info (succ char_code) ((char_code, new_glyph_info) :: acc)
  in
  get_glyph_info startcode []

let timing_test_opengl_works _ =
  let w =
    match
      Sdl.sdl_create_window "limitless" 0 0 800 800 Sdl.sdl_window_opengl
    with
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
  let _ = Sdl.sdl_pollevent () in
  for _ = 0 to 100_000 do
    Limitless.Opengl.gl_clear_color 1. 0. 0. 1.;
    Limitless.Opengl.gl_clear ();
    (match Sdl.sdl_gl_swapwindow w with Ok () -> () | Error e -> failwith e);
    Limitless.Opengl.gl_clear_color 0. 1. 0. 1.;
    Limitless.Opengl.gl_clear ();
    (match Sdl.sdl_gl_swapwindow w with Ok () -> () | Error e -> failwith e);
    Limitless.Opengl.gl_clear_color 0. 0. 1. 1.;
    Limitless.Opengl.gl_clear ();
    match Sdl.sdl_gl_swapwindow w with Ok () -> () | Error e -> failwith e
  done

let timing_test_drawing_rope _ =
  let gj = List.find (fun (c, _) -> Char.chr c = 'j') glyph_infos in
  let new_gj = (Char.chr (fst gj), snd gj) in
  let ropes = 1 in
  let all_j = List.init ropes (fun _ -> new_gj) in
  let biggest_horiBearingY =
    List.fold_left
      (fun acc (_, g) -> max g.FreeType.metrics.horiBearingY acc)
      0 glyph_infos
  in
  let rope =
    List.fold_left
      (fun acc gj ->
        match acc with
        | Some a -> Some (Limitless.Rope.concat a (Leaf [ gj ]))
        | None -> Some (Leaf [ gj ]))
      None all_j
    |> Option.get
  in
  ();
  let bigarray = Bigarray.Array1.create Float32 C_layout 1_000_000 in
  let start = Unix.gettimeofday () in
  let times = 500_000 in
  for _ = 0 to times do
    Render.draw_rope bigarray rope biggest_horiBearingY
  done;
  let end' = Unix.gettimeofday () -. start in
  assert_bool
    ("time it takes to draw " ^ Int.to_string ropes ^ " ropes/leaves "
   ^ Int.to_string times ^ " times")
    (end' < 0.5)

let gl_gen_one_buffer_test _ =
  let buffer = Limitless.Opengl.gl_gen_one_buffer () in
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
  >::: [
         (*"gl set up" >:: timing_test_opengl_works;*)
         (*"rope drawing time" >:: timing_test_drawing_rope;*)
         "gl_gen_one_buffer test" >:: gl_gen_one_buffer_test;
         "writing to bigarray time test" >:: timing_test_writing_bigarray;
       ]

let () = run_test_tt_main tests
