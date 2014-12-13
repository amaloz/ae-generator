open Core.Std
open AeOps

type mode_s = { decode_s : string; tag_s : string }
type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

let ocb = {
  decode_s = "INI INI SWAP MSG SWAP MSG TBC DUP OUT XOR SWAP TBC DUP OUT XOR FIN FIN";
  tag_s = "INI INI SWAP TBC OUT"
}

let _ =
  let usage_msg () = "Usage : " ^ Sys.argv.(0) ^ " [<args>]\n" in

  let arg_mode = ref "" in
  let arg_decode = ref "" in
  let arg_tag = ref "" in
  let arg_debug = ref 0 in
  let arg_display = ref false in
  let arg_eval = ref false in
  let arg_file = ref "" in
  let arg_check = ref false in
  let arg_remove_dups = ref false in

  let arg_specs = [
    ("-mode", Arg.Set_string arg_mode,
     "M  Load mode M");
    ("-decode", Arg.Set_string arg_decode,
     "A  Sets A to be the decode block");
    ("-tag", Arg.Set_string arg_tag,
     "A  Sets A to be the tag block");
    ("-display", Arg.Set arg_display,
     "Display mode as a graph (need 'dot' and 'feh')");
    ("-eval", Arg.Set arg_eval,
     "Evaluate the given mode");
    ("-file", Arg.Set_string arg_file,
     "FILE  Run against modes given in FILE");
    ("-check", Arg.Set arg_check,
     "Check if input mode(s) is/are secure");
    ("-debug", Arg.Set_int arg_debug,
     "N  Set debug level to N (0 ≤ N ≤ 4)");
    ("-remove-dups", Arg.Set arg_remove_dups,
     "Remove duplicate modes");
  ] in
  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());
  Utils.debug_config !arg_debug;

  let mode =
    if !arg_mode <> "" then
      match String.uppercase !arg_mode with
      | "OCB" -> ocb
      | _ -> begin
          Printf.printf "Error: Unknown mode '%s'\n" !arg_mode;
          exit 1
        end
    else if !arg_decode = "" || !arg_tag = "" then
      begin
        Printf.printf "Error: One of decode/tag algorithms is empty";
        exit 1
      end
    else
      { decode_s = !arg_decode; tag_s = !arg_tag }
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
    let display mode =
      AeGraph.display_with_feh mode.decode;
      AeGraph.display_with_feh mode.tag
    in
    let eval mode =
      Printf.printf "Encode = %s\n" (AeGraph.eval mode.encode);
      Printf.printf "Decode = %s\n" (AeGraph.eval mode.decode);
      Printf.printf "Tag    = %s\n" (AeGraph.eval mode.tag)
    in
    let check mode =
      let f g = AeGraph.is_secure g in
      f mode.decode && f mode.tag
    in
    if !arg_check then print_endline (if check mode then "yes" else "no");
    if !arg_eval then eval mode;
    if !arg_display then display mode;
  in

  match !arg_file with
  | "" -> run mode
  | _ -> failwith "not implemented yet"
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
  (*   Printf.printf "# found modes: %d\n" (List.length !blocks); *)
  (*   for i = 1 to !maxsize do *)
  (*     Printf.printf "# modes of size %d = %d\n%!" i (AeInst.count !blocks i) *)
  (*   done *)
