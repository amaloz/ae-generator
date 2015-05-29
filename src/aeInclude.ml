include AeOps
include Core.Std
module Lgr = Log

let version = "0.1"

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

let start command =
  Exn.handle_uncaught ~exit:true (fun () ->
      Command.run ~version:version command
    )
