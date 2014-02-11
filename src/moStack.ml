open Core.Std
open MoOps

let is_valid block =
  let eq x y = (Instruction x) = y in
  List.count block (eq Out) = 1
  && List.count block (eq M) = 1
  && List.count block (eq Start) = 1
  && List.count block (eq Nextiv_block) = 1
  && List.count block (eq Genrand) = 0
  && (List.count block (eq Prf) >= 1
      || List.count block (eq Prp) >= 1)

let is_pruneable i block =
  let cmp_prev i prev =
    let cmpi i = prev = Instruction i in
    let cmps i = prev = StackInstruction i in
    match i with
    | Instruction i ->
       begin
         match i with
         | Dup -> cmpi Dup
         | Inc -> cmpi M || cmpi Inc || cmpi Prp || cmpi Genrand
         | Out -> cmpi M || cmpi Inc || cmpi Genrand
         | Nextiv_block -> cmpi M
         | Prf | Prp -> cmpi Prf || cmpi Prp || cmpi Genrand
         | Xor -> cmpi Dup
         | Genrand | M | Nextiv_init | Start -> false
       end
    | StackInstruction i ->
       begin
         match i with
         | Swap -> cmpi Dup || cmps Swap
         | Twoswap -> cmps Twoswap
       end
  in
  let cmp_2prev i p p' =
    let cmpi_p i = p = Instruction i in
    let cmpi_p' i = p' = Instruction i in
    match i with
    | Instruction i ->
       begin
         match i with
         | Out -> cmpi_p' M && (cmpi_p Prp || cmpi_p Dup)
         | _ -> false
       end
    | _ -> false
  in
  match block with
  | hd :: hd' :: _ -> cmp_prev i hd || cmp_2prev i hd hd'
  | hd :: _ -> cmp_prev i hd
  | [] -> false
