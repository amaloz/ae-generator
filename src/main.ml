open Core.Std
module DLog = Log
open Async.Std
open Async_parallel.Std
open AeOps

let workers = ["localhost"; "localhost"]

let version = "0.1"

type mode = { encode : AeGraph.t; decode : AeGraph.t; tag : AeGraph.t }

let debug =
  Command.Spec.Arg_type.create
    (fun s ->
       let i = Int.of_string s in
       if i >= 0 && i <= 4 then i
       else begin
         failwith "Error: debug value out of range."
         (* eprintf "Error: debug value out of range.\n%!"; exit 1 *)
       end
    )

let mode =
  Command.Spec.Arg_type.create
    (fun s ->
       match String.Map.find Modes.modes (String.uppercase s) with
       | Some mode -> mode
       | None ->
         failwith (sprintf "Error: unknown mode '%s'. Available modes: %s."
                     s Modes.modes_string);
         (* eprintf "Error: unknown mode '%s'. Available modes: %s.\n%!" s *)
         (*   Modes.modes_string; *)
         (* exit 1 *)
    )

let spec_common =
  let open Command.Spec in
  empty
  +> flag "-debug" (optional_with_default 0 int (* debug *))
    ~doc:"N Set debug level to N (0 ≤ N ≤ 4)"

let spec_check =
  let open Command.Spec in
  empty
  +> flag "-mode" (optional mode)
    ~doc:(sprintf "M Load mode M (Available modes: %s)" Modes.modes_string)
  +> flag "-decode" (optional string)
    ~doc:"A Sets A to be the decode block"
  +> flag "-tag" (optional string)
    ~doc:"A Sets A to be the tag block"
  +> flag "-check" no_arg
    ~doc:" Check if mode is secure"
  +> flag "-display" no_arg
    ~doc:" Display mode as a graph (needs 'dot' and 'feh')"
  +> flag "-eval" no_arg
    ~doc:" Evaluate the given mode"
  (* +> flag "-file" (optional file) *)
  (*   ~doc:"F Run against modes given in F" *)
  ++ spec_common

let run_check mode decode tag check display eval debug () =
  Utils.debug_config debug;
  let mode =
    match mode with
    | Some mode -> mode
    | None -> begin
        match decode, tag with
        | Some decode, Some tag -> Modes.create decode tag
        | None, _ ->
          failwith "Error: decode algorithm is missing."
          (* eprintf "Error: decode algorithm is missing.\n%!"; exit 1 *)
        | _, None ->
          failwith "Error: tag algorithm is missing."
          (* eprintf "Error: tag algorithm is missing.\n%!"; exit 1 *)
      end
  in
  let run mode =
    DLog.info "Checking [%s] [%s]" (Modes.decode_string mode)
      (Modes.tag_string mode);
    let open Or_error.Monad_infix in
    let f str phase =
      AeInst.from_string_block str
      >>= fun block ->
      AeInst.validate block phase
      >>= fun () ->
      AeGraph.create block phase
    in
    f (Modes.decode_string mode) Decode
    >>= fun decode ->
    f (Modes.tag_string mode) Tag
    >>= fun tag ->
    AeGraph.derive_encode_graph decode
    >>= fun encode ->
    let mode = { encode = encode; decode = decode; tag = tag } in
    if check then begin
      let f g = AeGraph.is_secure g in
      let r = f mode.encode && f mode.decode && f mode.tag in
      print_endline (if r then "yes" else "no")
    end;
    if eval then begin
      printf "Encode = %s\n" (AeGraph.eval mode.encode);
      printf "Decode = %s\n" (AeGraph.eval mode.decode);
      printf "Tag    = %s\n" (AeGraph.eval mode.tag)
    end;
    if display then begin
      AeGraph.display_with_feh mode.decode;
      AeGraph.display_with_feh mode.encode;
      AeGraph.display_with_feh mode.tag
    end;
    if not check && not eval && not display then
      Or_error.error_string "One of -check, -display, or -eval must be used"
    else
      Ok ()
      (* eprintf "One of -check, -display, or -eval must be used\n%!"; exit 1 *)
  in
  match run mode with
  | Ok _ -> ()
  | Error err ->
    failwith (sprintf "Error: %s" (Error.to_string_hum err))
    (* eprintf "Error: %s\n%!" (Error.to_string_hum err); exit 1 *)

let spec_synth =
  let size = 11 in
  let open Command.Spec in
  empty
  +> flag "-all" no_arg
    ~doc:" Run for all block sizes less than or equal to the size given by -size"
  +> flag "-size" (optional_with_default size int)
    ~doc:(sprintf "N Number of instructions in decode to generate (default = %d)" size)
  +> flag "-print" no_arg
    ~doc:" Print found schemes to stdout"
  +> flag "-debug" (optional_with_default 0 debug)
    ~doc:"N Set debug level to N (0 ≤ N ≤ 4)"
  (* ++ spec_common *)

let run_synth all size print debug () =
  Utils.debug_config debug;
  let _ = AeGeneration.gen ~all ~print size Decode in
  Deferred.never ()

let check =
  Command.basic
    ~summary:"Authenticated encryption scheme prover."
    ~readme:(fun () -> sprintf "\
Proves a given authenticated encryption scheme secure.  The user can either
input an existing mode using the -mode flag, or input their own mode using the
-decode and -tag flags using following instructions:

  %s" (String.concat ~sep:", " all_ops))
    spec_check
    run_check

let synth =
  Command.async_basic
    ~summary:"Authenticated encryption scheme synthesizer."
    spec_synth
    run_synth

let command =
  Command.group
    ~summary:"Authenticated encryption scheme prover/synthesizer."
    ["check", check; "synth", synth]

let _ =
  Exn.handle_uncaught ~exit:true (fun () ->
      Parallel.init ~cluster:{ Cluster.master_machine = Unix.gethostname ();
                               worker_machines = workers
                             } ();
      Command.run ~version:version command
    )
