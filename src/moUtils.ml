open Core.Std

let debug_config n =
  Log.color_on();
  Log.set_log_level (match n with
                     | 0 -> Log.FATAL
                     | 1 -> Log.ERROR
                     | 2 -> Log.WARN
                     | 3 -> Log.INFO
                     | _ -> Log.DEBUG);
  Log.set_output stdout

let run_proc cmd =
  let bufsize = 128 in
  let buf = Buffer.create bufsize in
  let str = String.create bufsize in
  let chan = Unix.open_process_in cmd in
  let read = ref 1 in
  while !read <> 0 do
    read := input chan str 0 bufsize;
    Buffer.add_substring buf str 0 !read
  done;
  ignore (Unix.close_process_in chan);
  Buffer.contents buf

