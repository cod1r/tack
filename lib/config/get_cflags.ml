module C = Configurator.V1
let run_cmd cmd =
  let channel = Unix.open_process_in cmd in
  input_line channel
;;
let sdl2_output_cflags = run_cmd "sdl2-config --cflags" |> (String.split_on_char ' ');;
let sdl2_output_clibs = run_cmd "sdl2-config --libs" |> (String.split_on_char ' ');;
let freetype_output_cflags = run_cmd "pkg-config --cflags freetype2" |> (String.split_on_char ' ');;
let freetype_output_clibs = run_cmd "pkg-config --libs freetype2" |> (String.split_on_char ' ');;
let () =
  C.main ~name:"get_cflags" (fun _ ->
    let default : C.Pkg_config.package_conf =
      { libs   = sdl2_output_clibs @ freetype_output_clibs
      ; cflags = sdl2_output_cflags @ freetype_output_cflags
      } in
    C.Flags.write_sexp "c_flags.sexp" default.cflags;
    C.Flags.write_sexp "c_library_flags.sexp" default.libs);;
