open AeInclude

type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

let default_tag = "INI1 TBC OUT1 INI2"
let default_tag_simple = "INI1 TBC OUT1"

let debug =
  Command.Spec.Arg_type.create (fun s ->
      let i = Int.of_string s in
      if i >= 0 && i <= 4 then i
      else failwith "Error: debug value out of range.")

let mode =
  Command.Spec.Arg_type.create (fun s ->
      match String.Map.find AeModes.modes (String.uppercase s) with
      | Some mode -> mode
      | None ->
        failwithf "Error: unknown mode '%s'. Available modes: %s."
          s AeModes.modes_string ())

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
    ~doc:" Check if mode is secure"
  +> flag "-display" no_arg
    ~doc:" Display mode as a graph (needs 'dot' and 'feh')"
  +> flag "-eval" no_arg
    ~doc:" Evaluate the given mode"
  +> flag "-file" (optional file)
    ~doc:"FILE Run against modes in FILE"
  +> flag "-simple" no_arg
    ~doc:" Use simplified modes"
  +> flag "-save" no_arg
    ~doc:" Save displayed mode to a file"
  +> flag "-debug" (optional_with_default 0 debug)
    ~doc:"N Set debug level to N (0 ≤ N ≤ 4)"

let run_check mode decode tag check display eval file simple save debug () =
  set_log_level debug;
  let open Or_error.Monad_infix in
  let get_mode mode =
    match mode with
    | Some mode -> Ok mode
    | None -> begin
        match decode, tag with
        | Some decode, Some tag -> Ok (AeModes.create decode tag)
        | Some decode, None ->
          let tag = if simple then default_tag_simple else default_tag in
          printf "Warning: No tag algorithm given.  Using default tag algorithm: %s\n%!"
            tag;
          Ok (AeModes.create decode tag)
        | None, _ -> Or_error.error_string "Decode algorithm is missing."
      end
  in
  let fcheck mode =
    let f g = AeGraph.is_secure g ~simple in
    f mode.encode >>= fun _ ->
    f mode.decode >>= fun _ ->
    f mode.tag
  in
  let feval mode =
    printf "Encode = %s\n%!" (AeGraph.eval mode.encode);
    printf "Decode = %s\n%!" (AeGraph.eval mode.decode);
    printf "Tag    = %s\n%!" (AeGraph.eval mode.tag)
  in
  let fdisplay mode save =
    AeGraph.display mode.decode mode.encode mode.tag ~save in
  let run () =
    let run_mode mode =
      begin
        if check || eval || display then
          Ok ()
        else
          Or_error.error_string "One of -check, -display, or -eval must be used"
      end >>= fun () ->
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
      if display then ignore (fdisplay mode save);
      if check then begin
        match fcheck mode with
        | Ok _ -> printf "yes\n%!"
        | Error _ -> printf "no\n%!"
      end;
      if eval then feval mode;
      Ok ()
    in
    let read_file file =
      assert false
      (*   let total = ref 0 in *)
      (*   let blocks = ref [] in *)
      (*   let parse line = *)
      (*     if line = "" then () *)
      (*     else if String.contains ~pos:0 ~len:1 line '#' then () *)
      (*     else *)
      (*       let f c line = *)
      (*         try String.index_exn line c with *)
      (*         | Not_found -> failwith "Fatal: invalid format" *)
      (*       in *)
      (*       let a = f '(' line in *)
      (*       let b = f ')' line in *)
      (*       let decode = String.sub line (a + 1) (b - 1) in *)
      (*       let mode = AeModes.create decode default_tag in *)
      (*       let f mode =  *)
      (*         AeInst.from_string_block (AeModes.decode_string mode) >>= fun block -> *)
      (*         AeInst.validate block Decode ~simple                  >>| fun () -> *)
      (*         total := !total + 1; *)
      (*         blocks := block :: !blocks *)
      (*       in *)
      (*       let _ = f mode in () *)
      (*   in *)
      (*   In_channel.with_file file ~f:(fun ic -> *)
      (*       let stream = Stream.from (fun _ -> In_channel.input_line ic) in *)
      (*       Stream.iter parse stream *)
      (*     ); *)
      (*   let blocks = AeGeneration.remove_dups !blocks in *)
      (*   let unique = List.length blocks in *)
      (*   List.iter blocks ~f:(fun block -> *)
      (*       let mode = AeModes.create (string_of_op_list block) default_tag in *)
      (*       let _ = run_mode in () *)
      (*     ); *)
      (*   Ok () *)
    in
    match file with
    | Some file -> read_file file
    | None -> get_mode mode >>= fun mode -> run_mode mode
  in
  begin
    match run () with
    | Ok _ -> ()
    | Error err -> printf "Error: %s\n%!" (Error.to_string_hum err)
  end;
  shutdown ()

let spec_synth =
  let size = 11 in
  let open Command.Spec in
  empty
  +> flag "-all" no_arg
    ~doc:" Run for all block sizes less than or equal to the size given by -size"
  +> flag "-size" (optional_with_default size int)
    ~doc:(sprintf "N Number of instructions in decode to generate (default = %d)" size)
  +> flag "-print" no_arg
    ~doc:" Print found schemes to stdout"
  +> flag "-simple" no_arg
    ~doc:" Use simplified modes"
  +> flag "-debug" (optional_with_default 0 debug)
    ~doc:"N Set debug level to N (0 ≤ N ≤ 4)"
  (* ++ spec_common *)

let run_synth all size print simple debug () =
  set_log_level debug;
  let _ = AeGeneration.gen ~simple ~all ~print size Decode in
  shutdown ()

let check =
  Command.basic
    ~summary:"Authenticated encryption scheme prover."
    ~readme:(fun () -> sprintf "\
Proves a given authenticated encryption scheme secure.  The user can either
input an existing mode using the -mode flag, or input their own mode using the
-decode and -tag flags using following instructions:

  %s" (String.concat ~sep:", " all_ops))
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
