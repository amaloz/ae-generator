open Core.Std
open MoOps
module MoInst = MoInstructions

let _ =
  let usage_msg () = "Usage : " ^ Sys.argv.(0) ^ " [<args>]\n" in

  let arg_init = ref "GENRAND DUP OUT NEXTIV" in
  let arg_block = ref "" in
  let arg_display = ref false in
  let arg_display_model = ref false in
  (* let arg_eval = ref false in *)
  let arg_file = ref "" in
  let arg_save_smt = ref "" in
  let arg_validate = ref false in

  let arg_specs = [
    ("-init", Arg.Set_string arg_init,
     "INIT  Sets INIT to be the init block");
    ("-block", Arg.Set_string arg_block,
     "BLOCK  Sets BLOCK to be the main block");
    ("-display", Arg.Set arg_display,
     "Display mode as a graph (need 'dot' and 'feh')");
    ("-display-model", Arg.Set arg_display_model,
     "Display mode as a graph with correct assignments");
    (* ("-eval", Arg.Set arg_eval, *)
    (*  "Evaluate the given mode"); *)
    ("-file", Arg.Set_string arg_file,
     "FILE  Run against modes given in FILE");
    ("-save-smt", Arg.Set_string arg_save_smt,
     "FILE  Save SMT code created during validation to FILE");
    ("-validate", Arg.Set arg_validate,
     "Validate the given mode");
    ("-debug", Arg.Int MoUtils.set_debug_level,
     "N  Sets debug level to N");
  ] in
  Arg.parse arg_specs (fun _ -> ()) (usage_msg ());

  Log.color_on();
  Log.set_log_level Log.DEBUG;
  Log.set_output stdout;

  (* if !arg_display = false && !arg_eval = false && !arg_validate = false *)
  (* then begin *)
  (*   Printf.printf "one of -display, -eval, -validate must be set\n%!"; *)
  (*   exit 1 *)
  (* end; *)

  let file = match !arg_file with
    | "" -> None
    | s -> Some s in

  let display init block =
    let g = MoGraph.create init block in
    MoGraph.display_with_feh g
  in

  let validate init block =
    let g = MoGraph.create init block in
    let model = match !arg_display_model with
      | true -> Some g
      | false -> None in
    let save = match !arg_save_smt with
      | "" -> None
      | s -> Some s in
    if MoGraph.check ~save:save ~model:model g then
      print_endline "success!"
    else
      failwith "Unable to validate: SMT check failed!"
  in

  let run init block =
    Printf.printf "Checking [%s] [%s]\n%!" init block;
    let f x phase = MoInst.from_string_block (String.of_string x) phase in
    let init = f init Init in
    let block = f block Block in
    if !arg_validate then validate init block;
    if !arg_display then display init block;
  in

  match file with
    | None -> begin
      try run !arg_init !arg_block
      with Failure s -> print_endline s; exit 1
    end
    | Some fn -> begin
      let parse line =
        if line = "" then ()
        else if String.contains ~pos:0 ~len:1 line '#' then ()
        else
          let rec loop l l' phase =
            match l with
              | [] -> failwith "Fatal: should never get here!"
              | s :: ss ->
                if String.contains ~pos:0 ~len:(String.length s) s ']' then
                  let s = String.filter ~f:(fun c -> c <> ']') s in
                  ss, String.concat ~sep:" " (List.rev (s :: l'))
                else if String.contains ~pos:0 ~len:1 s '[' then
                  let s = String.filter ~f:(fun c -> c <> '[') s in
                  loop ss (s :: l') phase
                else
                  loop ss (s :: l') phase
          in
          let l = String.split line ~on:' ' in
          let l, init = loop l [] Init in
          let _, block = loop l [] Block in
          try run init block
          with Failure s -> print_endline s; exit 1
      in
      let ic = In_channel.create fn in
      let stream = Stream.from (fun _ -> In_channel.input_line ic) in
      Stream.iter parse stream;
      In_channel.close ic
    end
