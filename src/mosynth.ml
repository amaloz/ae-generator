open Core.Std
open MoOps
module MoInst = MoInstructions

let _ =
  let usage_msg () = "Usage: " ^ Sys.argv.(0) ^ " [<args>]\n" in

  let arg_init = ref "GENRAND DUP OUT NEXTIV" in
  let arg_block_depth = ref 6 in
  let arg_all = ref false in
  let arg_ops = ref "" in
  (* let arg_prepend = ref "" in *)

  let arg_specs = [
    ("-all", Arg.Set arg_all,
     "Run for all blocks <= block depth");
    ("-block-depth", Arg.Set_int arg_block_depth,
     "N  Number of instructions in the block to generate");
    ("-init", Arg.Set_string arg_init,
     "INIT  Sets INIT to be the init block (default = " ^ !arg_init ^ ")");
    ("-ops", Arg.Set_string arg_ops,
     "LIST  Sets ops in list to on (+) or off (-); e.g., \"+TPRP,-GENZERO\"");
    (* ("-prepend", Arg.Set_string arg_prepend, *)
    (*  "BLOCK  Prepends BLOCK to the generated block"); *)
    ("-debug", Arg.Int MoUtils.set_debug_level,
     "N  Set debug level to N");
  ] in
  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());

  Log.color_on();
  Log.set_log_level Log.DEBUG;
  Log.set_output stdout;
  
  (* let prepend = *)
  (*   match !arg_prepend with *)
  (*   | "" -> [] *)
  (*   | x -> MoInst.from_string_block x Block in *)

  let all = [
    "DUP"; "GENRAND"; (* "INC"; *) "M"; "NEXTIV"; "OUT"; "PRF"; "START"; "XOR";
    "SWAP"; "2SWAP"
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

  let block_count = ref 0 in
  let found_count = ref 0 in

  let init = MoInst.from_string_block (!arg_init) Init in
  (* let f l = String.concat ~sep:" " (List.map l MoInst.string_of_t) in *)
  let run block_depth =
    (* let eval g = *)
    (*   (\* if MoUtils.get_debug_level () = 2 then *\) *)
    (*   (\*   Log.infof "Trying [%s] [%s]" (f init) (f block); *\) *)
    (*   (\* let g = MoGraph.create init block |> MoGraph.assign_families in *\) *)
    (*   if MoGraph.validate g then *)
    (*     begin *)
    (*       found_count := !found_count + 1; *)
    (*       (\* Printf.printf "[%s] [%s]\n%!" (f init) (f block) *\) *)
    (*     end; *)
    (* in *)
    let found = MoGeneration.gengraphs init block_depth all in
    ()
    (* block_count := !block_count + (List.length graphs); *)
    (* List.iter graphs eval *)
  in

  let total = ref 0 in
  let inc_total x =
    let i2f x = Int.to_float x in
    let f2i x = Float.to_int x in
    let len = List.length all in
    (* let x = x - (List.length prepend) in *)
    total := !total + f2i (i2f len ** i2f x)
  in

  begin
    if !arg_all then
      for i = 1 to !arg_block_depth do
        inc_total i;
        run i
      done
    else begin
        inc_total !arg_block_depth;
        run !arg_block_depth
      end
  end;

  Printf.printf ": possible modes: %d\n" !total;
  Printf.printf ": potential modes: %d\n" !block_count;
  Printf.printf ": found modes: %d\n" !found_count

