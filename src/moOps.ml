
type phase = Init | Block

type instruction =
  | Dup
  | Genrand
  | Inc
  | M
  | Nextiv_init
  | Nextiv_block
  | Out
  | Prf
  | Xor

type stackInstruction =
  | Swap
  | Twoswap

type operations =
  | Instruction of instruction
  | StackInstruction of stackInstruction
  | Subroutine of string * operations list * int * int
