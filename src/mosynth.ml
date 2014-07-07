open Core.Std
open MoOps
module MoInst = MoInstructions

let range n =
  let rec f n = if n = 0 then [] else n :: (f (n - 1)) in
  f n |> List.rev

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

  let arg_all = ref false in
  let arg_block_size = ref 7 in
  let arg_decryptable_count = ref false in
  let arg_debug = ref 0 in
  let arg_disable_pruning = ref false in
  let arg_init = ref "GENRAND DUP OUT NEXTIV" in
  let arg_ops = ref "" in
  let arg_valid_count = ref false in

  let arg_specs = [
    ("-all", Arg.Set arg_all,
     "Run for all block sizes less than or equal to the size given by -block-size");
    ("-block-size", Arg.Set_int arg_block_size,
     "N  Number of instructions in the block to generate (default = "
     ^ (Int.to_string !arg_block_size) ^ ")");
    ("-valid-count", Arg.Set arg_valid_count,
     "Count schemes which are valid modes");
    ("-decryptable-count", Arg.Set arg_decryptable_count,
     "Count scheme which are decryptable");
    ("-init", Arg.Set_string arg_init,
     "INIT  Sets INIT to be the init block (default = " ^ !arg_init ^ ")");
    ("-ops", Arg.Set_string arg_ops,
     "LIST  Sets ops in list to on (+) or off (-); e.g., \"-XOR\"");
    ("-debug", Arg.Set_int arg_debug,
     "N  Set debug level to N (0 ≤ N ≤ 4)");
    ("-disable-pruning", Arg.Set arg_disable_pruning,
     "Disable pruning");
  ] in
  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());

  MoUtils.debug_config !arg_debug;
  
  let all = [
    "DUP"; "INC"; "M"; "NEXTIV"; "OUT"; "PRF"; "PRP"; "XOR"; "SWAP"; "2SWAP"
  ] in
  let all = instructions !arg_ops all in
  let init = MoInst.from_string_block (!arg_init) Init in
  (* duplicate items table *)
  let tbl = String.Table.create () ~size:1024 in
  let run f found block_size =
    let blocks = MoGeneration.gen f ~pruning:(not !arg_disable_pruning)
                                  init block_size all tbl in
    List.append found blocks
  in
  let f =
    if !arg_valid_count then
      (run MoGeneration.is_valid)
    else if !arg_decryptable_count then
      (run MoGeneration.is_decryptable)
    else
      (run MoGeneration.is_secure)
  in
  let found =
    if !arg_all then
      let sizes = range !arg_block_size in
      List.fold_left sizes ~init:[] ~f:f
    else
      f [] !arg_block_size
  in

  let bin_by_size l =
    let t = Int.Table.create () in
    let block_length l =
      let f acc = function
        | Instruction _ -> acc + 1
        | StackInstruction _ -> acc
      in
      List.fold_left l ~init:0 ~f:f
    in
    let f i =
      let len = block_length i in
      match Hashtbl.find t len with
      | None -> Hashtbl.add_exn t ~key:len ~data:1
      | Some cnt -> Hashtbl.replace t ~key:len ~data:(cnt + 1)
    in
    List.iter l f;
    let modesize size =
      let count =
        match Hashtbl.find t size with
        | None -> 0
        | Some cnt -> cnt
      in
      Printf.printf ": # modes of size %d = %d\n%!" size count
    in
    List.iter (range !arg_block_size) modesize
  in
  let bin_by_primitive l =
    let t = String.Table.create () in
    let has_prf l = List.exists l (fun i -> i = Instruction Prf) in
    let has_prp l = List.exists l (fun i -> i = Instruction Prp) in
    let inc s =
      match Hashtbl.find t s with
      | None -> Hashtbl.add_exn t ~key:s ~data:1
      | Some cnt -> Hashtbl.replace t ~key:s ~data:(cnt + 1)
    in
    let f i =
      let prf = has_prf i in
      let prp = has_prp i in
      match prf, prp with
      | true, true -> inc "mixture"
      | true, false -> inc "prf"
      | false, true -> inc "prp"
      | false, false -> failwith "no prf/prp in valid mode?!"
    in
    List.iter l f;
    let primitive s =
      let count =
        match Hashtbl.find t s with
        | None -> 0
        | Some cnt -> cnt
      in
      Printf.printf ": # modes of primitive %s = %d\n%!" s count
    in
    List.iter ["prf"; "prp"; "mixture"] primitive
  in
  List.iter found (fun l ->
                   Printf.printf "%s\n%!" (MoInst.string_of_t_list l));
  Printf.printf ": found modes: %d\n" (List.length found);
  bin_by_size found;
  bin_by_primitive found
