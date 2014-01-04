open Core.Std
module MoInst = MoInstructions

let _ =
  let usage_msg () = "Usage: " ^ Sys.argv.(0) ^ " filenames\n" in

  let fnames = ref [] in

  let arg_subroutines = ref "" in

  let arg_specs = [
    ("-subroutines", Arg.Set_string arg_subroutines,
     "FILE  Loads subroutines from FILE");
    ("-debug", Arg.Int MoUtils.set_debug_level,
     "N  Set debug level to N");
  ] in

  let f s = fnames := s :: !fnames in
  Arg.parse arg_specs f (usage_msg ());

  let subroutines =
    match !arg_subroutines with
      | "" -> String.Table.create ()
      | fn -> MoInst.load_subroutines fn in

  let parse_file fn =
    let modes = ref [] in
    let get_block s =
      String.slice s (String.index_exn s '[' + 1) (String.index_exn s ']') in
    let parse line =
      if line = "" then ()
      else if String.contains ~pos:0 ~len:1 line '#' then ()
      else
        let init = get_block line in
        let block = get_block (String.slice line
                                 (String.index_exn line ']' + 1) 0) in
        let init = MoInst.from_string_block init MoOps.Init subroutines in
        let block = MoInst.from_string_block block MoOps.Block subroutines in
        modes := (init, block) :: !modes
    in
    let ic = In_channel.create fn in
    let stream = Stream.from (fun _ -> In_channel.input_line ic) in
    Stream.iter parse stream;
    In_channel.close ic;
    !modes
  in

  let f acc fn = List.append acc (parse_file fn) in
  let modes = List.rev (List.fold !fnames ~init:[] ~f:f) in

  (* now we have a collection of all the modes we'd like to analyze *)

  (* 1. remove any duplicates *)
  let tbl = MoAnalyze.create_dup_tbl () in
  let modes = List.filter modes (fun m -> not (MoAnalyze.exists tbl m)) in

  (* 2. sort based on "performance metrics" *)
  let modes = List.sort modes ~cmp:MoAnalyze.cmp in

  let f (init, block) =
    MoInst.string_of_t_list init ^ " " ^ MoInst.string_of_t_list block in
  List.iter modes (fun x -> Printf.printf "%s\n" (f x));

  exit 0
