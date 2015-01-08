open Core.Std

type phase =
  | Encode
  | Decode
  | Tag

type instruction =
  | Msg1
  | Msg2
  | Ini1
  | Ini2
  | Fin1
  | Fin2
  | Out1
  | Out2
  | Dup
  | Xor
  | Tbc

type stackInstruction =
  | Swap
  | Twoswap

type op =
  | Instruction of instruction
  | StackInstruction of stackInstruction

let all_ops = [
  "MSG1"; "MSG2";
  "INI1"; "INI2";
  "FIN1"; "FIN2";
  "OUT1"; "OUT2";
  "DUP"; "XOR"; "TBC";
  "SWAP"; "2SWAP"
]

let string_of_phase = function
  | Encode -> "Encode"
  | Decode -> "Decode"
  | Tag -> "Tag"

let string_of_instruction = function
  | Msg1 -> "MSG1"
  | Msg2 -> "MSG2"
  | Ini1 -> "INI1"
  | Ini2 -> "INI2"
  | Fin1 -> "FIN1"
  | Fin2 -> "FIN2"
  | Out1 -> "OUT1"
  | Out2 -> "OUT2"
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
