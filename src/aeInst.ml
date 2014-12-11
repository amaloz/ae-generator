open Core.Std
open AeOps

type t = operations

let n_in = function
  | Instruction Msg -> 0
  | Instruction Ini -> 0
  | Instruction Fin -> 1
  | Instruction Out -> 1
  | Instruction Dup -> 1
  | Instruction Xor -> 2
  | Instruction Tbc -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let n_out = function
  | Instruction Msg -> 1
  | Instruction Ini -> 1
  | Instruction Fin -> 0
  | Instruction Out -> 0
  | Instruction Dup -> 2
  | Instruction Xor -> 1
  | Instruction Tbc -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let string_of_t = function
  | Instruction i ->
    begin
      match i with
      | Msg -> "MSG"
      | Ini -> "INI"
      | Fin -> "FIN"
      | Out -> "OUT"
      | Dup -> "DUP"
      | Xor -> "XOR"
      | Tbc -> "TBC"
    end
  | StackInstruction s ->
    begin
      match s with
      | Swap -> "SWAP"
      | Twoswap -> "2SWAP"
    end

let string_of_t_list l =
  List.to_string (fun x -> string_of_t x) l

let from_string s phase =
  match String.uppercase s with
  | "MSG" -> Instruction Msg
  | "INI" -> Instruction Ini
  | "FIN" -> Instruction Fin
  | "OUT" -> Instruction Out
  | "DUP" -> Instruction Dup
  | "XOR" -> Instruction Xor
  | "TBC" -> Instruction Tbc
  | "SWAP" -> StackInstruction Swap
  | "2SWAP" -> StackInstruction Twoswap
  | "" -> failwith "Failure: no instruction given"
  | _ as s -> failwith (String.concat ["Failure: unknown instruction "; s])

let from_string_block s phase =
  let f s = from_string s phase in
  List.map (String.split s ~on:' ') f

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
    let l = List.map l (fun block -> string_of_t_list block) in
    let l = List.sort ~cmp:String.compare l in
    List.iter l (fun block -> Printf.printf "%s\n" block);
  done
