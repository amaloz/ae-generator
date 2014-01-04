open Core.Std
open MoOps
module MoInst = MoInstructions

let genblock init ?(prepend = []) depth insts subroutines =

  let blocks = ref [] in
  let insts = List.append insts (String.Table.data subroutines) in
  let _, n_nextivs = MoInst.n_ins_and_outs init in

  let iter f depth ninputs block i =
    let n = ninputs - (MoInst.n_in i) + (MoInst.n_out i) in
    if MoInst.n_in i <= ninputs
    && n >= 0
    && not (MoStack.is_pruneable i block) then
      let block = match i with
        | Subroutine (_, l, _, _) -> List.append (List.rev l) block
        | _ -> i :: block in
      f (depth - 1) n block
  in

  let rec loop depth ninputs block =
    match depth with
      | 0 ->
        let block = List.rev block in
        if ninputs = 0
        && MoStack.is_valid block n_nextivs
        && MoGraph.check init block then
          blocks := block :: !blocks
      | _ ->
        List.iter insts (iter loop depth ninputs block)
  in

  let max_depth = depth in
  let rec loop_prepend depth ninputs block =
    match List.nth prepend (max_depth - depth) with
      | None -> List.iter insts (iter loop depth ninputs block)
      | Some i -> iter loop_prepend depth ninputs block i
  in

  loop_prepend depth n_nextivs [];
  !blocks
