open Core.Std
open AeOps

let version = "0.1"

type mode_s = { decode_s : string; tag_s : string }
type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

let ocb = {
  decode_s = "INI INI SWAP MSG SWAP MSG TBC DUP OUT XOR SWAP TBC DUP OUT XOR FIN FIN";
  tag_s = "INI INI SWAP TBC OUT"
}

let modes = "OCB"

let spec_check =
  let open Command.Spec in
  empty
  +> flag "-mode" (optional string)
    ~doc:(sprintf "M Load mode M (Available modes: %s)" modes)
  +> flag "-decode" (optional string) ~doc:"A Sets A to be the decode block"
  +> flag "-tag" (optional string) ~doc:"A Sets A to be the tag block"
  +> flag "-check" no_arg ~doc:"Check if mode is secure"
  +> flag "-display" no_arg ~doc:"Display mode as a graph (needs 'dot' and 'feh')"
  +> flag "-eval" no_arg ~doc:"Evaluate the given mode"
  +> flag "-file" (optional string) ~doc:"F Run against modes given in F"
  +> flag "-debug" (optional_with_default 0 int) ~doc:"N Set debug level to N (0 ≤ N ≤ 4)"

let run_check mode decode tag check display eval file debug () =
  Utils.debug_config debug;

  let mode =
    match mode with
    | Some mode -> begin
        match String.uppercase mode with
        | "OCB" -> ocb
        | _ ->
          printf "Error: Unknown mode '%s'\n" mode;
          exit 1
      end
    | None -> begin
        match decode, tag with
        | Some decode, Some tag ->
          { decode_s = decode; tag_s = tag }
        | _, _ ->
          printf "Error: One of decode/tag algorithms is empty\n";
          exit 1
      end
  in
  let str_to_mode mode =
    (* TODO: do some validity checks of decode and tag *)
    let f str phase = AeInst.from_string_block (String.of_string str) phase in
    let decode = AeGraph.create (f mode.decode_s Decode) Decode in
    let tag = AeGraph.create (f mode.tag_s Tag) Tag in
    let encode = AeGraph.derive_encode_graph decode in
    { encode = encode; decode = decode; tag = tag }
  in

  let run mode =
    Log.infof "Checking [%s] [%s]\n%!" mode.decode_s mode.tag_s;
    let mode = str_to_mode mode in
    if check then begin
      let f g = AeGraph.is_secure g in
      let r = f mode.decode && f mode.tag in
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
      print_endline "One of -check, -display, or -eval must be used"
  in

  match file with
  | None -> run mode
  | Some file -> failwith "not implemented yet"
  (* | fn -> *)
  (*   let blocks = ref [] in *)
  (*   let maxsize = ref 0 in *)
  (*   (\* duplicate items table *\) *)
  (*   let tbl = String.Table.create () ~size:1024 in *)
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
  (*       let block = String.sub line (a + 1) (b - 1) in *)
  (*       let size = (String.count block (fun c -> c = ' ')) + 1 in *)
  (*       if size > !maxsize then *)
  (*         maxsize := size; *)
  (*       try run !arg_init block with *)
  (*       | Success -> *)
  (*         if AeGeneration.exists *)
  (*             ~keep_dups:(not !arg_remove_dups) *)
  (*             tbl *)
  (*             (to_insts !arg_init Init) *)
  (*             (to_insts block Block) *)
  (*         then *)
  (*           () *)
  (*         else *)
  (*           let block = to_insts block Block in *)
  (*           blocks := block :: !blocks *)
  (*       | Failed -> () *)
  (*   in *)
  (*   let ic = In_channel.create fn in *)
  (*   let stream = Stream.from (fun _ -> In_channel.input_line ic) in *)
  (*   Stream.iter parse stream; *)
  (*   In_channel.close ic; *)
  (*   (\* print out relevant info *\) *)
  (*   AeInst.print_modes !blocks !maxsize; *)
  (*   printf "# found modes: %d\n" (List.length !blocks); *)
  (*   for i = 1 to !maxsize do *)
  (*     printf "# modes of size %d = %d\n%!" i (AeInst.count !blocks i) *)
  (*   done *)

let spec_synth =
  let open Command.Spec in
  empty

let run_synth () = failwith "Not implemented yet"

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

