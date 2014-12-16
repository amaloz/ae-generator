open Core.Std

type phase =
  | Encode
  | Decode
  | Tag

type instruction =
  | Msg
  | Ini
  | Fin
  | Out
  | Dup
  | Xor
  | Tbc

type stackInstruction =
  | Swap
  | Twoswap

type operations =
  | Instruction of instruction
  | StackInstruction of stackInstruction

let string_of_instruction = function
  | Msg -> "MSG"
  | Ini -> "INI"
  | Fin -> "FIN"
  | Out -> "OUT"
  | Dup -> "DUP"
  | Xor -> "XOR"
  | Tbc -> "TBC"

let string_of_stack_instruction = function
  | Swap -> "SWAP"
  | Twoswap -> "2SWAP"

let string_of_op = function
  | Instruction i -> string_of_instruction i
  | StackInstruction i -> string_of_stack_instruction i

let string_of_op_list l =
  List.to_string (fun x -> string_of_op x) l
