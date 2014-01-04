open Core.Std
open MoOps

type t = operations

let n_in = function
  | Instruction Double -> 1
  | Instruction Dup -> 1
  | Instruction Genrand -> 0
  | Instruction Genzero -> 0
  | Instruction Inc -> 1
  | Instruction M -> 0
  | Instruction Nextiv_init -> 1
  | Instruction Nextiv_block -> 1
  | Instruction Out -> 1
  | Instruction Prf -> 1
  | Instruction Prp -> 1
  | Instruction TPrp -> 2
  | Instruction Xor -> 2
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3
  | Subroutine (_, _, n, _) -> n

let n_out = function
  | Instruction Double -> 2
  | Instruction Dup -> 2
  | Instruction Genrand -> 1
  | Instruction Genzero -> 1
  | Instruction Inc -> 1
  | Instruction M -> 1
  | Instruction Nextiv_init -> 1
  | Instruction Nextiv_block -> 0
  | Instruction Out -> 0
  | Instruction Prf -> 1
  | Instruction Prp -> 1
  | Instruction TPrp -> 1
  | Instruction Xor -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3
  | Subroutine (_, _, _, n) -> n

let string_of_t = function
  | Instruction i -> begin
    match i with
      | Double -> "DOUBLE"
      | Dup -> "DUP"
      | Genrand -> "GENRAND"
      | Genzero -> "GENZERO"
      | Inc -> "INC"
      | M -> "M"
      | Nextiv_init | Nextiv_block -> "NEXTIV"
      | Out -> "OUT"
      | Prf -> "PRF"
      | Prp -> "PRP"
      | TPrp -> "TPRP"
      | Xor -> "XOR"
  end
  | StackInstruction s -> begin
    match s with
      | Swap -> "SWAP"
      | Twoswap -> "2SWAP"
  end
  | Subroutine (s, _, _, _) -> s

let string_of_t_list l =
  List.to_string (fun x -> string_of_t x) l

let from_string s phase subroutines =
  match String.uppercase s with
    | "DOUBLE" -> Instruction Double
    | "DUP" -> Instruction Dup
    | "GENRAND" -> Instruction Genrand
    | "GENZERO" -> Instruction Genzero
    | "INC" -> Instruction Inc
    | "M" -> Instruction M
    | "NEXTIV" -> begin
      match phase with
        | Init -> Instruction Nextiv_init
        | Block -> Instruction Nextiv_block
    end
    | "OUT" -> Instruction Out
    | "PRF" -> Instruction Prf
    | "PRP" -> Instruction Prp
    | "TPRP" -> Instruction TPrp
    | "XOR" -> Instruction Xor
    | "SWAP" -> StackInstruction Swap
    | "2SWAP" -> StackInstruction Twoswap
    | _ -> begin
      (* see if the instruction exists in our subroutines table *)
      match String.Table.find subroutines s with
        | Some s -> s
        | None -> raise (Failure (
          "Fatal: unknown instruction/subroutine '" ^ s ^ "'"))
    end

let from_string_block s phase subroutines =
  let f s = from_string s phase subroutines in
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
      | Subroutine (_, _, i, o) -> (i, o)
  in
  List.fold s ~init:(0,0) ~f:f

let load_subroutines fn =
  let load fn =
    let tbl = String.Table.create () in
    let parse line =
      if line = "" then ()
      else
        let name, block = String.lsplit2_exn line ~on:'=' in
        let strip s = String.strip s ~drop:(fun c -> c = ' ') in
        let name = strip name in
        let block = strip block in
        let block = from_string_block block Block tbl in
        let i, o = n_ins_and_outs block in
        String.Table.set tbl ~key:name ~data:(Subroutine (name, block, i, o))
    in
    let ic = In_channel.create fn in
    let stream = Stream.from (fun _ -> In_channel.input_line ic) in
    Stream.iter parse stream;
    In_channel.close ic;
    MoUtils.debug 1 "%s\n%!" ("Subroutine file '" ^ fn ^ "' successfully loaded");
    tbl in
  match fn with
    | "" -> String.Table.create ()
    | fn -> load fn

