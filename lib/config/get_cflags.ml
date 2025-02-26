module C = Configurator.V1

let run_cmd cmd =
  let channel = Unix.open_process_in cmd in
  input_line channel

let cmds =
  [
    "sdl2-config --cflags";
    "sdl2-config --libs";
    "pkg-config --cflags freetype2";
    "pkg-config --libs freetype2";
    "pkg-config --cflags harfbuzz";
    "pkg-config --libs harfbuzz";
  ]

let flags =
  List.map (fun s -> run_cmd s |> String.split_on_char ' ') cmds |> List.concat

let () =
  C.main ~name:"get_cflags" (fun _ ->
      let default : C.Pkg_config.package_conf =
        { libs = flags; cflags = flags }
      in
      C.Flags.write_sexp "c_flags.sexp" default.cflags;
      C.Flags.write_sexp "c_library_flags.sexp" default.libs)
