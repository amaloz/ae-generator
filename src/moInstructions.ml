open Core.Std
open MoOps

type t = operations

let n_in = function
  | Instruction Dup -> 1
  | Instruction Genrand -> 0
  | Instruction Inc -> 1
  | Instruction M -> 0
  | Instruction Nextiv_init -> 1
  | Instruction Nextiv_block -> 1
  | Instruction Out -> 1
  | Instruction Prf -> 1
  | Instruction Prp -> 1
  | Instruction Start -> 1
  | Instruction Xor -> 2
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let n_out = function
  | Instruction Dup -> 2
  | Instruction Genrand -> 1
  | Instruction Inc -> 1
  | Instruction M -> 1
  | Instruction Nextiv_init -> 1
  | Instruction Nextiv_block -> 0
  | Instruction Out -> 0
  | Instruction Prf -> 1
  | Instruction Prp -> 1
  | Instruction Start -> 1
  | Instruction Xor -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let string_of_t = function
  | Instruction i ->
    begin
      match i with
      | Dup -> "DUP"
      | Genrand -> "GENRAND"
      | Inc -> "INC"
      | M -> "M"
      | Nextiv_init | Nextiv_block -> "NEXTIV"
      | Out -> "OUT"
      | Prf -> "PRF"
      | Prp -> "PRP"
      | Start -> "START"
      | Xor -> "XOR"
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
  | "DUP" -> Instruction Dup
  | "GENRAND" -> Instruction Genrand
  | "INC" -> Instruction Inc
  | "M" -> Instruction M
  | "NEXTIV" ->
    begin
      match phase with
      | Init -> Instruction Nextiv_init
      | Block -> Instruction Nextiv_block
    end
  | "OUT" -> Instruction Out
  | "PRF" -> Instruction Prf
  | "PRP" -> Instruction Prp
  | "START" -> Instruction Start
  | "XOR" -> Instruction Xor
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
  done;
