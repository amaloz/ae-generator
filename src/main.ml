open AeInclude

type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

let default_tag = "INI1 TBC OUT1 INI2"
let default_tag_simple = "INI1 TBC OUT1"

let debug =
  Command.Spec.Arg_type.create (fun s ->
      let i = Int.of_string s in
      if i >= 0 && i <= 4 then i
      else let _ = eprintf "Error: debug value out of range.\n%!" in exit 1)

let mode =
  Command.Spec.Arg_type.create (fun s ->
      match String.Map.find AeModes.modes (String.uppercase s) with
      | Some mode -> mode
      | None ->
        let _ = eprintf "Error: unknown mode '%s'. Available modes: %s.\n%!"
            s AeModes.modes_string in exit 1)

let spec_common =
  let open Command.Spec in
  empty
  +> flag "-simple" no_arg
    ~doc:" Use simplified modes (no INI2 and FIN2 nodes)"
  +> flag "-debug" (optional_with_default 0 debug)
    ~doc:"N Set debug level to N (0 ≤ N ≤ 4)"

let spec_check =
  let open Command.Spec in
  empty
  +> flag "-mode" (optional mode)
    ~doc:(sprintf "M Load mode M (Available modes: %s)" AeModes.modes_string)
  +> flag "-decode" (optional string)
    ~doc:"A Sets A to be the decode block"
  +> flag "-tag" (optional string)
    ~doc:"A Sets A to be the tag block"
  +> flag "-check" no_arg
    ~doc:" Check if given mode is secure"
  +> flag "-display" no_arg
    ~doc:" Display given mode as a graph (needs 'gvpack', 'dot', and 'feh')"
  +> flag "-eval" no_arg
    ~doc:" Evaluate the given mode"
  +> flag "-file" (optional file)
    ~doc:"FILE Run against modes in FILE"
  +> flag "-save" (optional string)
    ~doc:"FILE Save mode to FILE instead of displaying"
  +> flag "-misuse" no_arg
    ~doc:" Check if given mode is misuse resistant"
  ++ spec_common

let run_check mode decode tag check display eval file save misuse simple debug () =
  set_log_level debug;
  let open Or_error.Monad_infix in
  let get_mode = function
    | Some mode -> Ok (mode, AeModes.decode_string mode)
    | None -> begin
        match decode, tag with
        | Some decode, Some tag -> Ok (AeModes.create decode tag, decode)
        | Some decode, None ->
          let tag = if simple then default_tag_simple else default_tag in
          printf "Warning: No tag algorithm given.  Using default tag algorithm: %s\n%!"
            tag;
          Ok (AeModes.create decode tag, decode)
        | None, _ -> Or_error.error_string "Decode algorithm is missing."
      end
  in
  let fcheck mode =
    let f g = AeGraph.is_secure g ~simple in
    f mode.encode >>= fun () ->
    f mode.decode >>= fun () ->
    f mode.tag
  in
  let fmisuse mode =
    if check then
      AeGraph.is_misuse_resistant mode.encode mode.decode mode.tag ~simple
    else
      Or_error.error_string "-misuse must be used in conjunction with -check"
  in
  let feval mode =
    printf "Encode: %s\n%!" (AeGraph.eval mode.encode ~simple);
    printf "Decode: %s\n%!" (AeGraph.eval mode.decode ~simple);
    printf "Tag:    %s\n%!" (AeGraph.eval mode.tag ~simple);
    Ok ()
  in
  let fdisplay mode save =
    AeGraph.display mode.decode mode.encode mode.tag ~save;
    Ok ()
  in
  let run_mode mode =
    Lgr.info "Checking [%s] [%s]" (AeModes.decode_string mode)
      (AeModes.tag_string mode);
    let f str phase =
      AeInst.from_string_block str        >>= fun block ->
      AeInst.validate block phase ~simple >>= fun () ->
      AeGraph.create block phase
    in
    f (AeModes.decode_string mode) Decode >>= fun decode ->
    f (AeModes.tag_string mode) Tag       >>= fun tag ->
    AeGraph.derive_encode_graph decode    >>= fun encode ->
    let mode = { encode = encode; decode = decode; tag = tag } in
    begin if display then fdisplay mode save else Ok () end >>= fun () ->
    begin if check then fcheck mode else Ok () end          >>= fun () ->
    begin if misuse then fmisuse mode else Ok () end        >>= fun () ->
    begin if eval then feval mode else Ok () end
  in
  let read_file file =
    let fold (count, acc) line =
      if line = "" then (count, acc)
      else if String.contains ~pos:0 ~len:1 line '#' then (count, acc)
      else
        let decode = String.strip ~drop:(fun c -> c = '\n') line in
        let mode = AeModes.create decode default_tag in
        let f mode =
          AeInst.from_string_block (AeModes.decode_string mode) >>= fun block ->
          AeInst.validate block Decode ~simple                  >>| fun () ->
          block
        in
        match f mode with
        | Ok block -> (count + 1, block :: acc)
        | Error _ -> (count + 1, acc)
    in
    let f ic = In_channel.fold_lines ic ~init:(0, []) ~f:fold in
    let total, blocks = In_channel.with_file file ~f in
    let blocks = AeGeneration.remove_dups blocks ~simple in
    let unique = List.length blocks in
    let f count block =
      let mode_s = string_of_op_list block in
      let tag = if simple then default_tag_simple else default_tag in
      let mode = AeModes.create mode_s tag in
      match run_mode mode with
      | Ok () -> printf "%s\n%!" mode_s; count + 1
      | Error err -> eprintf "%s: %s\n%!" mode_s (Error.to_string_hum err); count
    in
    let secure = List.fold ~init:0 blocks ~f in
    if check then
      printf "Secure / Unique / Total: %d / %d / %d\n%!" secure unique total
    else
      printf "Unique / Total: %d / %d\n%!" unique total;
    Ok ()
  in
  let run () =
    begin
      if check || display || eval then
        Ok ()
      else
        Or_error.error_string "One of -check, -display, -eval must be used"
    end >>= fun () ->
    match file with
    | Some file -> read_file file
    | None -> get_mode mode >>= fun (mode, decode) ->
      run_mode mode         >>| fun () ->
      printf "%s\n%!" decode
  in
  begin
    match run () with
    | Ok _ -> ()
    | Error err -> eprintf "Error: %s\n%!" (Error.to_string_hum err)
  end;
  shutdown ()

let spec_synth =
  let size = 12 in
  let open Command.Spec in
  empty
  +> flag "-size" (optional_with_default size int)
    ~doc:(sprintf "N Number of instructions in decode to generate (default: %d)" size)
  +> flag "-print" no_arg
    ~doc:" Print found schemes to stdout"
  ++ spec_common

let run_synth size print simple debug () =
  set_log_level debug;
  let _ = AeGeneration.gen ~simple ~print size Decode in
  shutdown ()

let check =
  Command.basic
    ~summary:"Authenticated encryption scheme prover."
    ~readme:(fun () -> sprintf "\
Proves a given authenticated encryption scheme secure.  The user can either
input an existing mode using the -mode flag, or input their own mode using the
-decode and -tag flags using following instructions:

  %s

Using the -simple flag will remove INI2 and FIN2 from the available instructions."
                (String.concat ~sep:", " all_ops))
    spec_check
    run_check

let synth =
  Command.basic
    ~summary:"Authenticated encryption scheme synthesizer."
    spec_synth
    run_synth

let command =
  Command.group
    ~summary:"Authenticated encryption scheme prover/synthesizer."
    ["check", check; "synth", synth]

let _ = start command
