open Tack
open OUnit2
open Editor

let cursor_positioning_on_mousedown_test _ =
  (* need to open up the freetype module because of font_height *)
  let open Tack.Freetype in
  ();
  let rope = Rope.of_string "h\n\nh\n" in
  let editor : Editor.editor =
    {
      rope = Some rope;
      cursor_pos = 0;
      holding_ctrl = false;
      vertical_scroll_y_offset = 0;
      highlight = None;
      config_info = Editor.recalculate_info_relating_to_config ();
      search_rope = None;
      list_options_rope = None;
    }
  in
  let rope_pos =
    Editor.find_closest_rope_pos_for_cursor_on_coords editor (200, 0)
  in
  assert_equal rope_pos 1;
  let rope_pos =
    Editor.find_closest_rope_pos_for_cursor_on_coords editor (500, 500)
  in
  assert_equal rope_pos (Tack.Rope.length rope)

let tests =
  "editor tests"
  >::: [
         "cursor positioning when clicking somewhere on the window"
         >:: cursor_positioning_on_mousedown_test;
       ]

let () = run_test_tt_main tests
