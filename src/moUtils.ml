open Core.Std

let debug_level = ref 0

let get_debug_level () = !debug_level
let set_debug_level n = debug_level := n

let debug n format =
  if !debug_level >= n
  then Format.printf format
  else Format.ifprintf Format.std_formatter format

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

