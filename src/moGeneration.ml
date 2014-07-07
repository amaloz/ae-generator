open Core.Std
open MoOps
module MoInst = MoInstructions

(* Check if a mode, given as graph 'g', already exists in duplicate items table
'tbl'. *)
let exists tbl init block =
  let r = MoStack.eval init block in
  Log.infof "Result = %s" r;
  match Hashtbl.existsi tbl ~f:(fun ~key:k ~data:v -> v = r) with
  | true -> true
  | false ->
     let _ = Hashtbl.add tbl ~key:r ~data:r in
     false

let is_valid g = MoGraph.is_valid g
let is_decryptable g = is_valid g && MoGraph.is_decryptable g
let is_secure g = MoGraph.check g

(* Generates modes satisfying input function 'f', using 'init' as the Init
block, 'depth' as the number of elements in each mode, 'insts' as the
instructions to use in generation, and 'tbl' as the duplicate items table.  The
'pruning' flag either enables or disables pruning. *)
let gen f ?(pruning=true) init depth insts tbl =
  let blocks = ref [] in
  let process init block =
    Log.infof "Trying [%s] [%s]"
              (MoInst.string_of_t_list init) (MoInst.string_of_t_list block);
    let g = MoGraph.create init block in
    if f g then
      begin
        Log.infof "It works!";
        if exists tbl init block then
          Log.infof "already exists..."
        else
          blocks := block :: !blocks
      end
  in
  let rec iter depth ninputs block i =
    let n = ninputs - (MoInst.n_in i) + (MoInst.n_out i) in
    if MoInst.n_in i <= ninputs
       && n >= 0
       && (not pruning || not (MoStack.is_pruneable i block)) then
      loop (depth - 1) n (i :: block)
  and loop depth ninputs block =
    match depth with
    | _ when depth < 0 -> ()
    | 0 ->
       let block = List.rev block in
       if ninputs = 0 && MoStack.is_valid block then
         process init block
    | _ when depth > 0 ->
       List.iter insts (iter depth ninputs block)
  in
  (* the '-1' is due to including the 'Start' instruction. *)
  List.iter insts (iter (depth - 1) 1 [Instruction Start]);
  !blocks
