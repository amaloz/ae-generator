open Core.Std
open MoOps
module MoInst = MoInstructions

let _ =
  let usage_msg () = "Usage: " ^ Sys.argv.(0) ^ " [<args>]\n" in

  let arg_init = ref "GENRAND DUP OUT NEXTIV" in
  let arg_block_depth = ref 6 in
  let arg_all = ref false in
  let arg_ops = ref "" in
  let arg_prepend = ref "" in
  let arg_subroutines = ref "" in

  let arg_specs = [
    ("-all", Arg.Set arg_all,
     "Run for all blocks <= block depth");
    ("-block-depth", Arg.Set_int arg_block_depth,
     "N  Number of instructions in the block to generate");
    ("-init", Arg.Set_string arg_init,
     "INIT  Sets INIT to be the init block (default = " ^ !arg_init ^ ")");
    ("-ops", Arg.Set_string arg_ops,
     "LIST  Sets ops in list to on (+) or off (-); e.g., \"+TPRP,-GENZERO\"");
    ("-prepend", Arg.Set_string arg_prepend,
     "BLOCK  Prepends BLOCK to the generated block");
    ("-subroutines", Arg.Set_string arg_subroutines,
     "FILE  Loads subroutines from FILE");
    ("-debug", Arg.Int MoUtils.set_debug_level,
     "N  Set debug level to N");
  ] in

  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());

  let subroutines = MoInst.load_subroutines !arg_subroutines in

  let prepend =
    match !arg_prepend with
      | "" -> []
      | x -> MoInst.from_string_block x Block subroutines in

  let all = [
    "DUP"; "GENRAND"; "INC"; "M"; "NEXTIV"; "OUT"; "PRF"; "XOR";
    "SWAP"; "2SWAP"
  ] in

  let parse_ops all s =
    let loop acc s =
      let s = String.uppercase s in
      let f s = String.slice s 1 0 in
      match String.prefix s 1 with
        | "+" ->
          let s = f s in
          if List.exists acc (fun x -> x = s) then
            acc
          else
            s :: acc
        | "-" ->
          let s = f s in
          List.filter acc (fun x -> x <> s)
        | _ -> raise (Failure (
          "Error: invalid format for -ops string"))
    in
    let l = String.split s ~on:',' in
    List.fold l ~init:all ~f:loop
  in

  let all =
    let all =
      match !arg_ops with
        | "" -> all
        | s -> parse_ops all !arg_ops in
    let f s = MoInst.from_string s Block subroutines in
    List.map all ~f:f
  in

  (* let tbl = MoAnalyze.create_dup_tbl () in *)

  let block_count = ref 0 in
  let found_count = ref 0 in

  let run block_depth =

    let rec eval init block =
      (* if MoUtils.get_debug_level () = 2 then begin *)
      (*   let f l = String.concat ~sep:" " (List.map l MoInst.string_of_t) in *)
      (*   MoUtils.debug 2 "Trying [%s] [%s]\n%!" (f init) (f block) *)
      (* end; *)
      if (try MoSmt.validate init block with _ -> false)
      then
        let g = MoGraph.create init block in
        if (try MoSmtRel.validate_related g init block with _ -> false) then
          found_count := !found_count + 1;
        let f l = String.concat ~sep:" " (List.map l MoInst.string_of_t) in
        Printf.printf "[%s] [%s]\n%!" (f init) (f block)
    (* and found init block = *)
    (*   if not (MoAnalyze.exists tbl (init, block)) then begin *)
    (*     found_count := !found_count + 1; *)
    (*     let f l = String.concat ~sep:" " (List.map l MoInst.string_of_t) in *)
    (*     Printf.printf "[%s] [%s]\n%!" (f init) (f block) *)
    (*   end *)
    in

    let init = MoInst.from_string_block (!arg_init) Init subroutines in
    let blocks =
      MoGeneration.genblock init block_depth all subroutines ~prepend:prepend in
    block_count := !block_count + (List.length blocks);
    List.iter blocks (eval init)
  in

  let total = ref 0 in
  let inc_total x =
    let i2f x = Int.to_float x in
    let f2i x = Float.to_int x in
    let len = List.length all in
    let x = x - (List.length prepend) in
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

