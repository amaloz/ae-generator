open Core.Std

type phase =
  | Encode
  | Decode
  | Tag

type inst =
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

type stackInst =
  | Swap
  | Twoswap

type op =
  | Inst of inst
  | StackInst of stackInst

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

let string_of_inst = function
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

let string_of_stack_inst = function
  | Swap -> "SWAP"
  | Twoswap -> "2SWAP"

let string_of_op = function
  | Inst i -> string_of_inst i
  | StackInst i -> string_of_stack_inst i

let string_of_op_list l =
  List.to_string string_of_op l
