
type phase = Init | Block

type instruction =
  | Dup
  | Genrand
  | M
  | Nextiv_init
  | Nextiv_block
  | Out
  | Prf
  | Start
  | Xor

type stackInstruction =
  | Swap
  | Twoswap

type operations =
  | Instruction of instruction
  | StackInstruction of stackInstruction
