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

let timing_test_drawing_rope _ =
  let gj = List.find (fun (c, _) -> Char.chr c = 'j') glyph_infos in
  let new_gj = (Char.chr (fst gj), snd gj) in
  let all_j = List.init 4_00 (fun _ -> new_gj) in
  let w =
    match
      Sdl.sdl_create_window "limitless" 0 0 800 800
        (sdl_window_resizable lor sdl_window_allow_highdpi)
    with
    | Some (Window { width; height; title; _ } as w) ->
        Printf.printf "Created window: %s %d %d" title width height;
        print_newline ();
        w
    | None -> failwith "unable to create window"
  in
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
  Sdl.sdl_create_renderer w sdl_renderer_software;
  let start = Unix.gettimeofday () in
  for _ = 0 to 100 do
    Render.draw_rope w rope biggest_horiBearingY
  done;
  let end' = Unix.gettimeofday () -. start in
  assert_bool "time it takes to draw 4_00 ropes/leaves" (end' < 0.0001)

let tests =
  "render tests" >::: [ "rope drawing time" >:: timing_test_drawing_rope ]

let () = run_test_tt_main tests
