open Core.Std
open AeOps

let version = "0.1"

type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

let all_instructions = [
  "MSG1"; "MSG2";
  "INI1"; "INI2";
  "FIN1"; "FIN2";
  "OUT1"; "OUT2";
  "DUP"; "XOR"; "TBC"
]

let debug =
  Command.Spec.Arg_type.create
    (fun s ->
       let i = Int.of_string s in
       if i >= 0 && i <= 4 then i
       else begin
         eprintf "Error: debug value out of range\n%!";
         exit 1
       end
    )

let mode =
  Command.Spec.Arg_type.create
    (fun s ->
       match String.Map.find Modes.modes (String.uppercase s) with
       | Some mode -> mode
       | None ->
         eprintf "Error: unknown mode '%s'\n%!" s;
         exit 1
    )

let spec_common =
  let open Command.Spec in
  empty
  +> flag "-debug" (optional_with_default 0 debug)
    ~doc:"N Set debug level to N (0 ≤ N ≤ 4)"

let spec_check =
  let open Command.Spec in
  empty
  +> flag "-mode" (optional mode)
    ~doc:(sprintf "M Load mode M (Available modes: %s)"
            (String.Map.keys Modes.modes |> String.concat ~sep:", "))
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
    ~doc:"F Run against modes given in F"
  ++ spec_common

let run_check mode decode tag check display eval file debug () =
  Utils.debug_config debug;
  let mode =
    match mode with
    | Some mode -> mode
    | None -> begin
        match decode, tag with
        | Some decode, Some tag -> Modes.create decode tag
        | None, _ ->
          eprintf "Error: decode algorithm is missing!\n%!";
          exit 1
        | _, None ->
          eprintf "Error: tag algorithm is missing!\n%!";
          exit 1
      end
  in
  let str_to_mode mode =
    let f str phase =
      let f str phase =
        let open Or_error.Monad_infix in
        AeInst.from_string_block str
        >>= fun block ->
        AeInst.validate block phase
        >>= fun () ->
        AeGraph.create block phase
      in
      match f str phase with
      | Ok graph -> graph
      | Error err ->
        eprintf "Error: %s\n%!" (Error.to_string_hum err);
        exit 1
    in
    let decode = f (Modes.decode_string mode) Decode in
    let tag = f (Modes.tag_string mode) Tag in
    match AeGraph.derive_encode_graph decode with
    | Ok encode ->
      { encode = encode; decode = decode; tag = tag }
    | Error err ->
      eprintf "Error: %s\n%!" (Error.to_string_hum err);
      exit 1
  in
  let run mode =
    Log.info "Checking [%s] [%s]" (Modes.decode_string mode)
      (Modes.tag_string mode);
    let mode = str_to_mode mode in
    if check then begin
      let f g = AeGraph.is_secure g in
      let r = f mode.encode && f mode.decode && f mode.tag in
      print_endline (if r then "yes" else "no")
    end;
    if eval then begin
      printf "Encode = %s\n" (AeGraph.eval mode.encode);
      printf "Decode = %s\n" (AeGraph.eval mode.decode);
      printf "Tag    = %s\n" (AeGraph.eval mode.tag)
    end;
    if display then begin
      AeGraph.display_with_feh mode.decode;
      AeGraph.display_with_feh mode.encode;
      AeGraph.display_with_feh mode.tag
    end;
    if not check && not eval && not display then
      eprintf "One of -check, -display, or -eval must be used\n%!"
  in
  match file with
  | None -> run mode
  | Some file -> failwith "not implemented yet"

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
  ++ spec_common

let run_synth all size print debug () =
  Utils.debug_config debug;
  let insts = List.map all_instructions (fun i -> AeInst.from_string i)
              |> Or_error.combine_errors |> ok_exn in
  let tbl = String.Table.create () ~size:1024 in
  let found = AeGeneration.gen ~all:all size insts tbl Decode in
  if print then
    AeInst.print_modes found size;
  printf "# found modes: %d\n" (List.length found);
  for i = 1 to size do
    printf "# modes of size %d = %d\n%!" i (AeInst.count found i)
  done

let check =
  Command.basic
    ~summary:"Authenticated encryption scheme prover."
    ~readme:(fun () -> sprintf "\
Proves a given authenticated encryption scheme secure.  The user can either
input an existing mode using the -mode flag, or input their own mode using the
-decode and -tag flags using following instructions:

  %s" (String.concat ~sep:", " all_instructions))
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

let _ =
  Command.run ~version:version command
