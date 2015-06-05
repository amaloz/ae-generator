open AeInclude

type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

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
        let _ = eprintf "%s" (Error.to_string_hum err) in exit 1
    )

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
  +> flag "-file" (optional file)
    ~doc:"FILE Run against modes in FILE"
  +> flag "-parallel" no_arg
    ~doc:" Check if given mode is parallelizable"
  +> flag "-strict" no_arg
    ~doc:" Use strict check for parallelizability"
  +> flag "-cost" (optional int)
    ~doc:"N Filters out modes with cost greater than N"
  +> flag "-save" (optional string)
    ~doc:"FILE Save mode to FILE instead of displaying"
  (* +> flag "-misuse" no_arg *)
  (*   ~doc:" Check if given mode is misuse resistant" *)
  ++ spec_common

let run_check mode encode decode tag check display eval file parallel strict cost save simple debug () =
  set_log_level debug;
  let open Or_error.Monad_infix in
  let get_mode = function
    | Some mode -> Ok mode
    | None -> begin
        match encode, decode, tag with
        | Some encode, Some decode, _ ->
          Or_error.error_string "Only one of encode or decode algorithm can be given."
        | Some encode, None, Some tag -> Ok (AeModes.create encode tag)
        | None, Some decode, Some tag ->
          assert false
        | None, Some decode, None ->
          assert false
        | Some encode, None, None ->
          let tag = if simple then default_tag_simple else default_tag in
          Ok (AeModes.create encode tag)
        | None, None, _ -> Or_error.error_string "Encode algorithm is missing."
      end
  in
  let fcheck mode =
    let f g = AeGraph.is_secure g ~simple in
    f mode.encode >>= fun () ->
    f mode.decode >>= fun () ->
    f mode.tag    >>= fun () ->
    if parallel then
      if AeGraph.is_parallel mode.encode strict ~simple
         && AeGraph.is_parallel mode.decode strict ~simple
      then Ok ()
      else Or_error.errorf "Not %s parallelizable." (if strict then "strongly" else "weakly")
    else Ok ()
  in
  (* let fmisuse mode = *)
  (*   if check then *)
  (*     AeGraph.is_misuse_resistant mode.encode mode.decode mode.tag ~simple *)
  (*   else *)
  (*     Or_error.error_string "-misuse must be used in conjunction with -check" *)
  (* in *)
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
  let run_mode mode =
    Lgr.info "Checking %s" (AeModes.to_string mode);
    let f block phase =
      AeInst.validate block phase ~simple >>= fun () ->
      AeGraph.create block phase
    in
    f (AeModes.encode mode) Encode  >>= fun encode ->
    f (AeModes.tag mode) Tag        >>= fun tag ->
    AeGraph.reverse encode ~simple  >>= fun decode ->
    let mode = { encode = encode; decode = decode; tag = tag } in
    begin if display then fdisplay mode save else Ok () end >>= fun () ->
    begin if check then fcheck mode else Ok () end          >>= fun () ->
    begin if eval then feval mode else Ok () end
  in
  let read_file file =
    let fold (count, acc) line =
      if line = "" then (count, acc)
      else if String.contains ~pos:0 ~len:1 line '#' then (count, acc)
      else
        let parse line =
          let encode = String.strip ~drop:(fun c -> c = '\n') line in
          AeInst.from_string_block encode       >>= fun encode ->
          AeInst.validate encode Encode ~simple >>= fun () ->
          begin
            match cost with
            | None -> Ok ()
            | Some cost ->
              let cost' = AeInst.count_inst encode Tbc in
              if cost' > cost
              then Or_error.errorf "Cost too large (%d > %d)" cost' cost
              else Ok ()
          end                                   >>| fun () ->
          encode
        in
        match parse line with
        | Ok encode -> (count + 1, encode :: acc)
        | Error _ -> (count + 1, acc)
    in
    let f ic = In_channel.fold_lines ic ~init:(0, []) ~f:fold in
    let total, blocks = In_channel.with_file file ~f in
    let blocks = AeGeneration.remove_dups blocks ~simple in
    let unique = List.length blocks in
    if check || display || eval then
      let minsize = ref Int.max_value in
      let maxsize = ref 0 in
      let found = ref [] in
      let f count block =
        let tag = if simple then default_tag_simple else default_tag in
        let mode = AeModes.create block tag in
        match run_mode mode with
        | Ok () ->
          printf "%s\n%!" (AeModes.to_string mode);
          let size = List.length block in
          if size > !maxsize then maxsize := size;
          if size < !minsize then minsize := size;
          found := block :: !found;
          count + 1
        | Error err ->
          eprintf "%s: %s\n%!" (AeModes.to_string mode) (Error.to_string_hum err);
          count
      in
      let secure = List.fold ~init:0 blocks ~f in
      printf "Secure / Unique / Total: %d / %d / %d\n%!" secure unique total;
      let count blocks size =
        List.count blocks (fun block -> List.length block = size)
      in
      for i = !minsize to !maxsize do
        printf "# modes of size %d = %d\n%!" i (count !found i)
      done;
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
    | None -> get_mode mode >>= fun mode ->
      run_mode mode         >>| fun () ->
      printf "%s\n%!" (AeModes.encode mode |> string_of_op_list)
  in
  begin
    match run () with
    | Ok _ -> ()
    | Error err -> eprintf "Error: %s\n%!" (Error.to_string_hum err)
  end

let spec_synth =
  let size = 12 in
  let open Command.Spec in
  empty
  +> flag "-size" (optional_with_default size int)
    ~doc:(sprintf "N Number of instructions in encode to generate (default: %d)" size)
  +> flag "-print" no_arg
    ~doc:" Print found schemes to stdout"
  ++ spec_common

let run_synth size print simple debug () =
  set_log_level debug;
  let _ = AeGeneration.gen ~simple ~print size in
  ()

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
