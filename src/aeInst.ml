open Core.Std
open AeOps

exception Parse_error of string

let n_in = function
  | Instruction Msg1 -> 0
  | Instruction Msg2 -> 0
  | Instruction Ini1 -> 0
  | Instruction Ini2 -> 0
  | Instruction Fin1 -> 1
  | Instruction Fin2 -> 1
  | Instruction Out1 -> 1
  | Instruction Out2 -> 1
  | Instruction Dup -> 1
  | Instruction Xor -> 2
  | Instruction Tbc -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let n_out = function
  | Instruction Msg1 -> 1
  | Instruction Msg2 -> 1
  | Instruction Ini1 -> 1
  | Instruction Ini2 -> 1
  | Instruction Fin1 -> 0
  | Instruction Fin2 -> 0
  | Instruction Out1 -> 0
  | Instruction Out2 -> 0
  | Instruction Dup -> 2
  | Instruction Xor -> 1
  | Instruction Tbc -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let from_string s =
  match String.uppercase s with
  | "MSG1" -> Instruction Msg1
  | "MSG2" -> Instruction Msg2
  | "INI1" -> Instruction Ini1
  | "INI2" -> Instruction Ini2
  | "FIN1" -> Instruction Fin1
  | "FIN2" -> Instruction Fin2
  | "OUT1" -> Instruction Out1
  | "OUT2" -> Instruction Out2
  | "DUP" -> Instruction Dup
  | "XOR" -> Instruction Xor
  | "TBC" -> Instruction Tbc
  | "SWAP" -> StackInstruction Swap
  | "2SWAP" -> StackInstruction Twoswap
  | "" -> raise (Parse_error "no instruction given")
  | _ as s ->
    raise (Parse_error (sprintf "unknown instruction '%s'" s))

let from_string_block s =
  List.map (String.split s ~on:' ') from_string

let block_length l =
  (* we do not count stack instructions as part of the block length *)
  let f acc = function
    | Instruction _ -> acc + 1
    | StackInstruction _ -> acc
  in
  List.fold_left l ~init:0 ~f:f

(* counts the number of modes of size 'size' in list 'found' *)
let count found size =
  List.count found (fun l -> block_length l = size)

let print_modes found maxsize =
  for i = 1 to maxsize do
    Printf.printf "# modes of length %d:\n" i;
    let l = List.filter found (fun block -> block_length block = i) in
    let l = List.map l (fun block -> string_of_op_list block) in
    let l = List.sort ~cmp:String.compare l in
    List.iter l (fun block -> Printf.printf "%s\n" block);
  done
