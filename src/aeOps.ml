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
