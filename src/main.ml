open AeInclude

let version = "1.0"

type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

let set_log_level n =
  let level = match n with
    | 0 -> Lgr.FATAL
    | 1 -> Lgr.ERROR
    | 2 -> Lgr.WARN
    | 3 -> Lgr.INFO
    | _ -> Lgr.DEBUG
  in
  Lgr.set_log_level level;
  Lgr.color_on ()

let default_tag = [Inst Ini1; Inst Tbc; Inst Out1; Inst Ini2]
let default_tag_simple = [Inst Ini1; Inst Tbc; Inst Out1]

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

let scheme =
  Command.Spec.Arg_type.create (fun s ->
      match AeInst.from_string_block s with
      | Ok block -> block
      | Error err ->
        let _ = eprintf "%s" (Info.to_string_hum_deprecated (Base.Error.to_info err)) in exit 1)

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
    ~doc:(sprintf "M Load mode M (given as decode algorithm) [Available modes: %s]" AeModes.modes_string)
  +> flag "-encode" (optional scheme)
    ~doc:"A Sets A to be the encode block"
  +> flag "-decode" (optional scheme)
    ~doc:"A Sets A to be the decode block"
  +> flag "-tag" (optional scheme)
    ~doc:"A Sets A to be the tag block"
  +> flag "-check" no_arg
    ~doc:" Check if given mode is secure"
  +> flag "-display" no_arg
    ~doc:" Display given mode as a graph (needs 'gvpack', 'dot', and 'feh')"
  +> flag "-eval" no_arg
    ~doc:" Evaluate the given mode"
  +> flag "-dec-file" (optional string)
    ~doc:"FILE Run against modes (given as decode algorithms) in FILE"
  +> flag "-enc-file" (optional string)
    ~doc:"FILE Run against modes (given as encode algorithms) in FILE"
  +> flag "-parallel" no_arg
    ~doc:" Check if given mode is (weakly) parallelizable"
  +> flag "-strong" no_arg
    ~doc:" Use strong parallelizability check"
  +> flag "-forward" no_arg
    ~doc:  "Check if TBC calls use only the forward direction"
  +> flag "-cost" (optional int)
    ~doc:"N Filters out modes with cost greater than N"
  +> flag "-save" (optional string)
    ~doc:"FILE Save mode to FILE instead of displaying"
  +> flag "-attack" no_arg
    ~doc:" Check if given mode has an attack"
  ++ spec_common

let run_check mode encode decode tag check display eval dec_file enc_file
    parallel strong forward cost save attack simple debug () =
  set_log_level debug;
  let open Or_error.Monad_infix in
  let get_mode = function
    | Some mode -> Ok mode
    | None -> begin
        match encode, decode, tag with
        | Some _, Some _, _ ->
          Or_error.errorf "Only one of -encode, -decode can be used"
        | Some block, None, _
        | None, Some block, _ ->
          let tag = match tag with
            | Some tag -> tag
            | None -> if simple then default_tag_simple else default_tag
          in
          Ok (AeModes.create block tag)
        | None, None, _ ->
          Or_error.errorf "One of -encode, -decode must be used"
      end
  in
  let get_phase = function
    | Some mode -> Ok Decode
    | None -> begin
        match encode, decode with
        | Some _, Some _ ->
          Or_error.errorf "Only one of -encode, -decode can be used"
        | Some _, None -> Ok Encode
        | None, Some _ -> Ok Decode
        | None, None ->
          Or_error.errorf "One of -encode, -decode must be used"
      end
  in
  let check_parallel mode =
    if AeGraph.is_parallel mode.encode strong ~simple
       && AeGraph.is_parallel mode.decode strong ~simple
    then Ok ()
    else Or_error.errorf "Not %s parallelizable"
        (if strong then "strongly" else "weakly")
  in
  let check_forward mode =
    if AeGraph.check_direction mode.encode then Ok ()
    else Or_error.errorf "Uses backward direction of TBC"
  in
  let check_cost mode =
    match cost with
    | None -> Ok ()
    | Some cost ->
      let cost' = AeGraph.count mode.encode Tbc in
      if cost' <= cost then Ok ()
      else Or_error.errorf "Cost too large (%d > %d)" cost' cost
  in
  let prune mode =
    check_cost mode                                   >>= fun () ->
    (if parallel then check_parallel mode else Ok ()) >>= fun () ->
    (if forward then check_forward mode else Ok ())
  in
  let fattack mode =
    if AeGraph.is_attack_dec mode.encode mode.decode ~simple then
      Or_error.errorf "Attack found"
    else Ok ()
  in

  let fcheck mode =
    let f g = AeGraph.is_secure g ~simple in
    f mode.encode >>= fun () ->
    f mode.decode >>= fun () ->
    f mode.tag
  in
  let feval mode =
    let msg1, msg2 = AeGraph.msg1, AeGraph.msg2 in
    let eval mode =
      AeGraph.eval mode ~simple ~msg1 ~msg2 |> String.concat ~sep:" " in
    printf "Encode: %s\n%!" (eval mode.encode);
    printf "Decode: %s\n%!" (eval mode.decode);
    printf "Tag:    %s\n%!" (eval mode.tag);
    Ok ()
  in
  let fdisplay mode save =
    AeGraph.display mode.encode mode.decode mode.tag ~save;
    Ok ()
  in
  let run_mode mode phase =
    Lgr.info "Checking %s" (AeModes.to_string mode);
    let f block phase =
      AeInst.validate block phase ~simple >>= fun () ->
      AeGraph.create block phase
    in
    begin
      match phase with
      | Encode ->
        f (AeModes.block mode) Encode >>= fun encode ->
        f (AeModes.tag mode) Tag      >>= fun tag ->
        AeGraph.reverse encode        >>| fun decode ->
        encode, decode, tag
      | Decode ->
        f (AeModes.block mode) Decode >>= fun decode ->
        f (AeModes.tag mode) Tag      >>= fun tag ->
        AeGraph.reverse decode        >>| fun encode ->
        encode, decode, tag
      | Tag -> assert false
    end                               >>= fun (encode, decode, tag) ->
    let mode = { encode; decode; tag } in
    prune mode                                              >>= fun () ->
    begin if display then fdisplay mode save else Ok () end >>= fun () ->
    begin if attack then fattack mode else Ok () end        >>= fun () ->
    begin if check then fcheck mode else Ok () end          >>= fun () ->
    begin if eval then feval mode else Ok () end
  in
  let read_file file phase =
    assert (attack || check || display || eval);
    let fold (count, acc) line =
      if line = "" || String.contains ~pos:0 ~len:1 line '#' then (count, acc)
      else
        let parse line =
          let block = String.strip ~drop:(fun c -> c = '\n') line in
          AeInst.from_string_block block      >>= fun block ->
          AeInst.validate block phase ~simple >>| fun () ->
          block
        in
        match parse line with
        | Ok block -> (count + 1, block :: acc)
        | Error _ -> (count + 1, acc)
    in
    let f ic = In_channel.fold_lines ic ~init:(0, []) ~f:fold in
    let total, blocks = In_channel.with_file file ~f in
    let use_enc = match phase with
      | Encode -> true | Decode -> false | Tag -> assert false in
    let blocks = AeSynth.remove_dups blocks ~use_enc ~simple in
    let unique = List.length blocks in

    let minsize = ref Int.max_value in
    let maxsize = ref 0 in
    let found = ref [] in
    let f count block =
      let tag = if simple then default_tag_simple else default_tag in
      let mode = AeModes.create block tag in
      match run_mode mode phase with
      | Ok () ->
        printf "%s\n%!" (string_of_op_list (AeModes.block mode));
        let size = List.length block in
        if size > !maxsize then maxsize := size;
        if size < !minsize then minsize := size;
        found := block :: !found;
        count + 1
      | Error err ->
        eprintf "%s: %s\n%!" (AeModes.to_string mode) (Info.to_string_hum_deprecated (Base.Error.to_info err));
        count
    in
    let secure = List.fold ~init:0 blocks ~f in
    printf "# Secure / Unique / Total: %d / %d / %d\n%!" secure unique total;
    let count blocks size =
      List.count blocks (fun block -> List.length block = size)
    in
    for i = !minsize to !maxsize do
      printf "# modes of size %d = %d\n%!" i (count !found i)
    done;
    Ok ()
  in
  let run () =
    begin
      if attack || check || display || eval then Ok ()
      else Or_error.errorf "One of -attack, -check, -display, -eval must be used"
    end >>= fun () ->
    match enc_file, dec_file with
    | Some _, Some _ ->
      Or_error.errorf "Only one of -dec-file, -enc-file can be used\n%!"
    | Some file, None -> read_file file Encode
    | None, Some file -> read_file file Decode
    | None, None -> get_mode mode >>= fun mode' ->
      get_phase mode              >>= fun phase ->
      run_mode mode' phase        >>| fun () ->
      printf "Ok: %s\n%!" (AeModes.block mode' |> string_of_op_list)
  in
  match run () with
  | Ok _ -> ()
  | Error err -> eprintf "Error: %s\n%!" (Info.to_string_hum_deprecated (Base.Error.to_info err))

let spec_synth =
  let size = 12 in
  let open Command.Spec in
  empty
  +> flag "-encode" no_arg
    ~doc:" Synthesize encode algorithms"
  +> flag "-decode" no_arg
    ~doc:" Synthesize decode algorithms"
  +> flag "-size" (optional_with_default size int)
    ~doc:(sprintf "N Number of instructions in encode to generate (default: %d)" size)
  +> flag "-print" no_arg
    ~doc:" Print found schemes to stdout"
  +> flag "-attack" no_arg
    ~doc:" Synthesize schemes that we cannot find attacks to (rather than schemes we find secure)"
  ++ spec_common

let run_synth encode decode size print attack simple debug () =
  set_log_level debug;
  let open Or_error.Monad_infix in
  let run () =
    begin
      match encode, decode with
      | true, true -> Or_error.errorf "Only one of -encode, -decode can be used"
      | true, false -> Ok true
      | false, true -> Ok false
      | false, false -> Or_error.errorf "One of -encode, -decode must be used"
    end >>| fun use_enc ->
    AeSynth.synth ~use_enc ~simple ~print ~attack size
  in
  match run () with
  | Ok _ -> ()
  | Error err -> eprintf "Error: %s\n%!" (Info.to_string_hum_deprecated (Base.Error.to_info err))

let check =
  Command.basic_spec
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
  Command.basic_spec
    ~summary:"Authenticated encryption scheme synthesizer."
    spec_synth
    run_synth

let command =
  Command.group
    ~summary:"Authenticated encryption scheme prover/synthesizer."
    ["check", check; "synth", synth]

let _ =
  Exn.handle_uncaught ~exit:true (fun () ->
      Command_unix.run ~version:version command
    )
