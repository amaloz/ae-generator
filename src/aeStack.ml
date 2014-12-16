open Core.Std
open AeOps

let is_valid block =
  let eq x y = (Instruction x) = y in
  List.count block (eq Ini) = 2
  && List.count block (eq Fin) = 2
  && List.count block (eq Msg) = 2
  && List.count block (eq Out) = 2
  && List.exists block (eq Tbc)

let is_pruneable i block =
  let cmp_prev i prev =
    let cmpi i = prev = Instruction i in
    let cmpsi i = prev = StackInstruction i in
    match i with
    | Instruction i ->
      begin
        match i with
        | Out -> cmpi Msg
        | Tbc -> cmpi Tbc
        | Xor -> cmpi Dup
        | _ -> false
      end
    | StackInstruction i ->
      begin
        match i with
        | Swap -> cmpi Dup || cmpsi Swap
        | Twoswap -> cmpsi Twoswap
      end
  in
  match block with
  | hd :: _ -> cmp_prev i hd
  | [] -> false
