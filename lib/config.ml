  let read_config () =
    let config_str =
      In_channel.with_open_bin ".config.json" (fun ic ->
          In_channel.input_all ic)
    in
    Yojson.Safe.from_string config_str
