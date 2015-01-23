include AeOps
include Core.Std
(* open Async.Std *)
(* open Async_parallel.Std *)
(* open Rpc_parallel.Std *)
module Lgr = Log

let version = "0.1"

(* let set_log_level n = *)
(*   let level = match n with *)
(*     | 0 -> `Error *)
(*     | 1 -> `Info *)
(*     | _ -> `Debug *)
(*   in *)
(*   Log.Global.set_level level *)

(* let shutdown () = *)
(*   Shutdown.shutdown; *)
(*   Deferred.never () *)

(* let start command = *)
(*   (\* Parallel.start_app command *\) *)
(*   Exn.handle_uncaught ~exit:true (fun () -> *)
(*       Parallel.init (); *)
(*       Command.run ~version:version command *)
(*     ) *)

let set_log_level n =
  let level = match n with
    | 0 -> Lgr.FATAL
    | 1 -> Lgr.ERROR
    | 2 -> Lgr.WARN
    | 3 -> Lgr.INFO
    | _ -> Lgr.DEBUG
  in
  Lgr.set_log_level level;
  Lgr.color_on ()

let shutdown () = ()

let start command =
  Exn.handle_uncaught ~exit:true (fun () ->
      Command.run ~version:version command
    )
