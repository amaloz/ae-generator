open Core.Std
open MoOps

type t = operations

let n_in = function
  | Instruction Dup -> 1
  | Instruction Genrand -> 0
  | Instruction M -> 0
  | Instruction Nextiv_init -> 1
  | Instruction Nextiv_block -> 1
  | Instruction Out -> 1
  | Instruction Prf -> 1
  | Instruction Start -> 1
  | Instruction Xor -> 2
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let n_out = function
  | Instruction Dup -> 2
  | Instruction Genrand -> 1
  | Instruction M -> 1
  | Instruction Nextiv_init -> 1
  | Instruction Nextiv_block -> 0
  | Instruction Out -> 0
  | Instruction Prf -> 1
  | Instruction Start -> 1
  | Instruction Xor -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let string_of_t = function
  | Instruction i -> begin
    match i with
      | Dup -> "DUP"
      | Genrand -> "GENRAND"
      | M -> "M"
      | Nextiv_init | Nextiv_block -> "NEXTIV"
      | Out -> "OUT"
      | Prf -> "PRF"
      | Start -> "START"
      | Xor -> "XOR"
  end
  | StackInstruction s -> begin
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
    | "M" -> Instruction M
    | "NEXTIV" -> begin
      match phase with
        | Init -> Instruction Nextiv_init
        | Block -> Instruction Nextiv_block
    end
    | "OUT" -> Instruction Out
    | "PRF" -> Instruction Prf
    | "START" -> Instruction Start
    | "XOR" -> Instruction Xor
    | "SWAP" -> StackInstruction Swap
    | "2SWAP" -> StackInstruction Twoswap
    | "" -> raise (Failure "Failure: no instruction given")
    | _ as s -> raise (Failure (String.concat ["Failure: unknown instruction "; s]))

let from_string_block s phase =
  let f s = from_string s phase in
  List.map (String.split s ~on:' ') f

let mod_stack i s =
  match i with
    | Swap ->
      let x = Stack.pop_exn s in
      let x' = Stack.pop_exn s in
      Stack.push s x;
      Stack.push s x'
    | Twoswap ->
      let x = Stack.pop_exn s in
      let x' = Stack.pop_exn s in
      let x'' = Stack.pop_exn s in
      Stack.push s x;
      Stack.push s x';
      Stack.push s x''

let n_ins_and_outs s =
  let f (i, o) inst =
    match inst with
      | Instruction _ ->
        let ii = n_in inst in
        let io = n_out inst in
        let new_ii = if ii > o then i + ii - o else i in
        let new_io = if ii > o then io else o - ii + io in
        (new_ii, new_io)
      | StackInstruction _ -> (i, o)
  in
  List.fold s ~init:(0,0) ~f:f
