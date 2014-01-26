open Core.Std
open MoOps
module MoInst = MoInstructions

let _ =
  let usage_msg () = "Usage: " ^ Sys.argv.(0) ^ " [<args>]\n" in

  let arg_init = ref "GENRAND DUP OUT NEXTIV" in
  let arg_block_depth = ref 6 in
  let arg_all = ref false in
  let arg_ops = ref "" in

  let arg_specs = [
    ("-all", Arg.Set arg_all,
     "Run for all blocks <= block depth");
    ("-block-depth", Arg.Set_int arg_block_depth,
     "N  Number of instructions in the block to generate");
    ("-init", Arg.Set_string arg_init,
     "INIT  Sets INIT to be the init block (default = " ^ !arg_init ^ ")");
    ("-ops", Arg.Set_string arg_ops,
     "LIST  Sets ops in list to on (+) or off (-); e.g., \"-XOR\"");
    ("-debug", Arg.Int MoUtils.set_debug_level,
     "N  Set debug level to N");
  ] in
  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());

  Log.color_on();
  Log.set_log_level Log.INFO;
  Log.set_output stdout;
  
  let all = [
    "DUP"; "M"; "NEXTIV"; "OUT"; "PRF"; "PRP"; "XOR"; "SWAP"; "2SWAP"
  ] in
  let all =
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
    let all = match !arg_ops with
      | "" -> all
      | s -> parse_ops all !arg_ops in
    Log.infof "Supported instructions: %s" (List.to_string ident all);
    let f s = MoInst.from_string s Block in
    List.map all ~f:f
  in
  let num_total = ref 0 in
  let inc_total x =
    let len = List.length all in
    num_total := !num_total + (Int.to_float len ** Int.to_float x |> Float.to_int)
  in
  let found = ref [] in
  let init = MoInst.from_string_block (!arg_init) Init in
  let run block_depth =
    inc_total block_depth;
    let blocks = MoGeneration.gengraphs init block_depth all in
    found := List.append !found blocks
  in

  if !arg_all then
    for i = 1 to !arg_block_depth do run i done
  else
    run !arg_block_depth;

  List.iter !found (fun l ->
                    Printf.printf "%s\n%!" (MoInst.string_of_t_list l));
  
  Printf.printf ": possible modes: %d\n" !num_total;
  Printf.printf ": found modes: %d\n" (List.length !found)

