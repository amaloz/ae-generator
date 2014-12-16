open Core.Std
open AeOps

let version = "0.1"

type mode_s = { decode_s : string; tag_s : string }
type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

let ocb = {
  decode_s =
    "INI INI SWAP MSG SWAP MSG TBC DUP OUT XOR SWAP TBC DUP OUT XOR FIN FIN";
  tag_s =
    "INI INI SWAP TBC OUT"
}

let modes = "OCB"

let block =
  Command.Spec.Arg_type.create
    (fun s ->
       let block =
         try
           AeInst.from_string_block s
         with AeInst.Parse_error e ->
           eprintf "Error: parsing block failed: %s\n%!" e;
           exit 1
       in
       if AeStack.is_valid block then
         s
       else begin
         eprintf "Error: block is invalid\n%!";
         exit 1
       end
    )

let debug =
  Command.Spec.Arg_type.create
    (fun s ->
       let i = Int.of_string s in
       if i >= 0 && i <= 4 then
         i
       else begin
         eprintf "Error: debug value out of range\n%!";
         exit 1
       end
    )

let mode =
  Command.Spec.Arg_type.create
    (fun s ->
       match String.uppercase s with
       | "OCB" -> ocb
       | _ ->
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
    ~doc:(sprintf "M Load mode M (Available modes: %s)" modes)
  +> flag "-decode" (optional block)
    ~doc:"A Sets A to be the decode block"
  +> flag "-tag" (optional block)
    ~doc:"A Sets A to be the tag block"
  +> flag "-check" no_arg
    ~doc:"Check if mode is secure"
  +> flag "-display" no_arg
    ~doc:"Display mode as a graph (needs 'dot' and 'feh')"
  +> flag "-eval" no_arg
    ~doc:"Evaluate the given mode"
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
        | Some decode, Some tag ->
          { decode_s = decode; tag_s = tag }
        | _, _ ->
          eprintf "Error: One of decode/tag algorithms is empty\n%!";
          exit 1
      end
  in
  let str_to_mode mode =
    (* TODO: do some validity checks of decode and tag *)
    let f str = AeInst.from_string_block (String.of_string str) in
    let decode = AeGraph.create (f mode.decode_s) Decode in
    let tag = AeGraph.create (f mode.tag_s) Tag in
    let encode = AeGraph.derive_encode_graph decode in
    { encode = encode; decode = decode; tag = tag }
  in
  let run mode =
    Log.info "Checking [%s] [%s]\n%!" mode.decode_s mode.tag_s;
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
  let insts = [
    "INI"; "FIN"; "MSG"; "OUT"; "XOR"; "DUP"; "TBC"; "SWAP"; "2SWAP"
  ] in
  let insts = List.map insts (fun i -> AeInst.from_string i) in
  let tbl = String.Table.create () ~size:1024 in
  let run f found size =
    let blocks = AeGeneration.gen f size insts tbl Decode in
    List.append found blocks
  in
  let f = run (fun g -> AeGraph.is_secure g) in
  let found =
    if all then
      let sizes = List.range 10 (size + 1) in
      List.fold_left sizes ~init:[] ~f:f
    else
      f [] size
  in
  if print then
    AeInst.print_modes found size;
  printf "# found modes: %d\n" (List.length found);
  for i = 1 to size do
    printf "# modes of size %d = %d\n%!" i (AeInst.count found i)
  done

let check =
  Command.basic
    ~summary:"Authenticated encryption scheme prover"
    spec_check
    run_check

let synth =
  Command.basic
    ~summary:"Authenticated encryption scheme synthesizer"
    spec_synth
    run_synth

let command =
  Command.group ~summary:"Authenticated encryption scheme prover/synthesizer"
    ["check", check; "synth", synth]

let _ =
  Command.run ~version:version command
