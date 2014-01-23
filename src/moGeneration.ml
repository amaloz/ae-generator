open Core.Std
open MoOps
module MoInst = MoInstructions

let gengraphs init depth insts =
  let blocks = ref [] in
  let f l = String.concat ~sep:" " (List.map l MoInst.string_of_t) in
  let rec iter depth ninputs block i =
    let n = ninputs - (MoInst.n_in i) + (MoInst.n_out i) in
    if MoInst.n_in i <= ninputs
       && n >= 0
       && not (MoStack.is_pruneable i block) then
      loop (depth - 1) n (i :: block)
  and loop depth ninputs block =
    match depth with
    | 0 ->
       let block = List.rev block in
       if ninputs = 0 && MoStack.is_valid block then
         begin
           if MoUtils.get_debug_level () = 2 then
             Log.infof "Trying [%s] [%s]" (f init) (f block);
           let g = MoGraph.create init block in
           if MoGraph.check g then
             begin
               Log.infof "It works!";
               blocks := 1 :: !blocks
             end
         end
    | _ ->
       List.iter insts (iter depth ninputs block)
  in
  List.iter insts (iter depth 1 []);
  List.fold !blocks ~init:0 ~f:(fun x i -> x + i)
  
  (* let rec loop_prepend depth ninputs block = *)
  (*   match List.nth prepend (max_depth - depth) with *)
  (*     | None -> List.iter insts (iter loop depth ninputs block) *)
  (*     | Some i -> iter loop_prepend depth ninputs block i *)
  (* in *)
  (* loop_prepend depth 1 []; *)
  (* !blocks *)
