open Core.Std
open MoOps
module MoInst = MoInstructions

exception Success

let _ =
  let usage_msg () = "Usage : " ^ Sys.argv.(0) ^ " [<args>]\n" in

  let arg_init = ref "GENRAND DUP OUT NEXTIV" in
  let arg_block = ref "" in
  let arg_debug = ref 0 in
  let arg_display = ref false in
  let arg_eval = ref false in
  let arg_file = ref "" in
  let arg_is_valid = ref false in
  let arg_is_decryptable = ref false in
  let arg_is_secure = ref false in

  let arg_specs = [
    ("-init", Arg.Set_string arg_init,
     "INIT  Sets INIT to be the init block");
    ("-block", Arg.Set_string arg_block,
     "BLOCK  Sets BLOCK to be the main block");
    ("-display", Arg.Set arg_display,
     "Display mode as a graph (need 'dot' and 'feh')");
    ("-eval", Arg.Set arg_eval,
     "Evaluate the given mode");
    ("-file", Arg.Set_string arg_file,
     "FILE  Run against modes given in FILE");
    ("-is-valid", Arg.Set arg_is_valid,
     "Check if input mode(s) is/are valid");
    ("-is-decryptable", Arg.Set arg_is_decryptable,
     "Check if input mode(s) is/are decryptable");
    ("-is-secure", Arg.Set arg_is_secure,
     "Check if input mode(s) is/are secure");
    ("-debug", Arg.Set_int arg_debug,
     "N  Set debug level to N (0 ≤ N ≤ 4)");
  ] in
  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());

  MoUtils.debug_config !arg_debug;

  let display init block =
    MoGraph.create init block |> MoGraph.display_with_feh in
  let eval init block =
    Printf.printf "%s\n%!" (MoStack.eval init block); in
  let is f init block =
    if f (MoGraph.create init block) then raise Success
    else raise (Failure "") in
  let is_valid init block =
    is (fun g -> MoGraph.is_valid g) init block in
  let is_decryptable init block =
    is (fun g -> MoGraph.is_valid g && MoGraph.is_decryptable g) init block in
  let is_secure init block =
    is (fun g -> MoGraph.is_valid g && MoGraph.is_decryptable g
                 && MoGraph.is_secure g) init block in

  let to_insts str phase =
    MoInst.from_string_block (String.of_string str) phase
  in
  let run init block =
    Log.infof "Checking [%s] [%s]\n%!" init block;
    let init = to_insts init Init in
    let block = to_insts block Block in
    if !arg_is_valid then is_valid init block;
    if !arg_is_decryptable then is_decryptable init block;
    if !arg_is_secure then is_secure init block;
    if !arg_eval then eval init block;
    if !arg_display then display init block;
  in

  match !arg_file with
  | "" ->
     begin
       (try run !arg_init !arg_block with
        | Success -> print_endline "yes"
        | Failure _ -> print_endline "no");
       exit 1
     end
  | fn ->
     let blocks = ref [] in
     let maxsize = ref 0 in
     let parse line =
       if line = "" then ()
       else if String.contains ~pos:0 ~len:1 line '#' then ()
       else
         let f c line =
           try String.index_exn line c with
           | Not_found -> failwith "Fatal: invalid format"
         in
         let a = f '(' line in
         let b = f ')' line in
         let block = String.sub line (a + 1) (b - 1) in
         let size = (String.count block (fun c -> c = ' ')) + 1 in
         if size > !maxsize then
           maxsize := size;
         try run !arg_init block with
         | Success ->
            let block = to_insts block Block in
            blocks := block :: !blocks
         | Failure _ -> ()
     in
     let ic = In_channel.create fn in
     let stream = Stream.from (fun _ -> In_channel.input_line ic) in
     Stream.iter parse stream;
     In_channel.close ic;
     (* print out relevant info *)
     MoInst.print_modes !blocks !maxsize;
     Printf.printf "# found modes: %d\n" (List.length !blocks);
     for i = 1 to !maxsize do
       Printf.printf "# modes of size %d = %d\n%!" i (MoInst.count !blocks i)
     done
