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
  let arg_save_smt = ref "" in
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
    ("-save-smt", Arg.Set_string arg_save_smt,
     "FILE  Save SMT code created during validation to FILE");
    ("-is-decryptable", Arg.Set arg_is_decryptable,
     "Check if input mode(s) is/are decryptable");
    ("-is-secure", Arg.Set arg_is_secure,
     "Check if input mode(s) is/are secure");
    ("-debug", Arg.Set_int arg_debug,
     "N  Sets debug level to N");
  ] in
  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());

  MoUtils.debug_config !arg_debug;

  (* if !arg_display = false && !arg_eval = false && !arg_is_decryptable = false *)
  (* then begin *)
  (*   Printf.printf "one of -display, -eval, -validate must be set\n%!"; *)
  (*   exit 1 *)
  (* end; *)

  let file = match !arg_file with
    | "" -> None
    | s -> Some s in

  let display init block =
    MoGraph.create init block |> MoGraph.display_with_feh
  in

  let eval init block =
    let r = MoStack.eval init block in
    Printf.printf "Eval: %s\n%!" r;
  in

  let is_decryptable init block =
    let g = MoGraph.create init block in
    if MoGeneration.is_decryptable g then
      raise Success
    else
      raise (Failure "")
  in

  let is_secure init block =
    let g = MoGraph.create init block in
    let save = match !arg_save_smt with
      | "" -> None
      | s -> Some s in
    if MoGraph.check ~save:save g then
      raise Success
    else
      raise (Failure "")
  in

  let run init block =
    Log.infof "Checking [%s] [%s]\n%!" init block;
    let f x phase = MoInst.from_string_block (String.of_string x) phase in
    let init = f init Init in
    let block = f block Block in
    if !arg_is_decryptable then is_decryptable init block;
    if !arg_is_secure then is_secure init block;
    if !arg_eval then eval init block;
    if !arg_display then display init block;
  in

  match file with
  | None ->
     begin
       (try run !arg_init !arg_block with
        | Success -> print_endline "success"
        | Failure s -> print_endline "failure");
       exit 1
     end
  | Some fn ->
     let blocks = ref [] in
     let maxsize = ref 0 in
     begin
       let parse line =
         if line = "" then ()
         else if String.contains ~pos:0 ~len:1 line '#' then ()
         else
           let a =
             try String.index_exn line '(' with
             | Not_found -> failwith "Fatal: invalid format"
           in
           let b =
             try String.index_exn line ')' with
             | Not_found -> failwith "Fatal: invalid format"
           in
           let block = String.sub line (a + 1) (b - 1) in
           let size = (String.count block (fun c -> c = ' ')) + 1 in
           if size > !maxsize then
             maxsize := size;
           try run !arg_init block with
           | Success ->
              let block = MoInst.from_string_block (String.of_string block) Block in
              blocks := block :: !blocks
           | Failure s -> ()
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
     end
