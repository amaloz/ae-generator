open Core.Std
open MoOps
module MoInst = MoInstructions

let instructions l all =
  let parse_ops all s =
    let loop lst s =
      let s = String.uppercase s in
      let rest s = String.slice s 1 0 in
      match String.prefix s 1 with
      | "+" ->
         let s = rest s in
         if List.exists lst (fun x -> x = s) then
           lst
         else
           s :: lst
      | "-" ->
         let s = rest s in
         List.filter lst (fun x -> x <> s)
      | _ -> failwith "Error: invalid format for -ops string"
    in
    let l = String.split s ~on:',' in
    List.fold l ~init:all ~f:loop
  in
  let all = match l with
    | "" -> all
    | s -> parse_ops all l in
  Log.infof "Supported instructions: %s" (List.to_string ident all);
  let f s = MoInst.from_string s Block in
  List.map all ~f:f
                  
let _ =
  let usage_msg () = "Usage: " ^ Sys.argv.(0) ^ " [<args>]\n" in

  let arg_init = ref "GENRAND DUP OUT NEXTIV" in
  let arg_block_size = ref 7 in
  let arg_all = ref false in
  let arg_ops = ref "" in

  let arg_specs = [
    ("-all", Arg.Set arg_all,
     "Run for all block sizes less than or equal to the size given by -num");
    ("-block-size", Arg.Set_int arg_block_size,
     "N  Number of instructions in the block to generate (default = "
     ^ (Int.to_string !arg_block_size) ^ ")");
    ("-init", Arg.Set_string arg_init,
     "INIT  Sets INIT to be the init block (default = " ^ !arg_init ^ ")");
    ("-ops", Arg.Set_string arg_ops,
     "LIST  Sets ops in list to on (+) or off (-); e.g., \"-XOR\"");
    ("-debug", Arg.Int MoUtils.set_debug_level,
     "N  Set debug level to N");
  ] in
  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());

  Log.color_on();
  Log.set_log_level Log.DEBUG;
  Log.set_output stdout;
  
  let all = [
    "DUP"; "M"; "NEXTIV"; "OUT"; "PRF"; "PRP"; "XOR"; "SWAP"; "2SWAP"
  ] in
  let all = instructions !arg_ops all in
  let init = MoInst.from_string_block (!arg_init) Init in
  let run found block_size =
    (* XXX: breaks if block_size = 1 *)
    let blocks = MoGeneration.gengraphs init block_size all in
    List.append found blocks
  in
  let range n = 
    let rec f n = if n = 1 then [] else n :: (f (n - 1)) in
    f n |> List.rev
  in
  let found =
    if !arg_all then
      let sizes = range !arg_block_size in
      List.fold_left sizes ~init:[] ~f:run
    else
      run [] !arg_block_size
  in
  let num_total =
    let len = List.length all |> Int.to_float in
    let f acc x = acc + (len ** Int.to_float x |> Float.to_int) in
    let sizes = if !arg_all then range !arg_block_size else [!arg_block_size] in
    List.fold_left sizes ~init:0 ~f:f
  in

  List.iter found (fun l ->
                   Printf.printf "%s\n%!" (MoInst.string_of_t_list l));
  
  Printf.printf ": possible modes: %d\n" num_total;
  Printf.printf ": found modes: %d\n" (List.length found)

