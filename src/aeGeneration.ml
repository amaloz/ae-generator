open Core.Std
open AeOps

(* Check if a mode already exists in duplicate items table 'tbl'. *)
let exists ?(keep_dups=false) tbl g =
  if keep_dups then false
  else
    let r = AeGraph.eval g in
    Log.infof "Result = %s\n" r;
    match Hashtbl.add tbl ~key:r ~data:r with
    | `Duplicate -> true
    | `Ok -> false

(* Generates modes satisfying input function 'f', using 'depth' as the number of
   elements in each mode, 'insts' as the instructions to use in generation, and
   'tbl' as the duplicate items table. *)
let gen f depth insts tbl phase =
  let blocks = ref [] in
  let process block =
    Log.infof "Trying [%s]" (string_of_op_list block);
    let g = AeGraph.create block phase in
    if f g then
      begin
        Log.infof "It works!";
        if exists tbl g then
          Log.infof "already exists..."
        else
          blocks := block :: !blocks
      end
  in
  let rec iter depth ninputs block i =
    (* n contains the number of available inputs after 'i' is used *)
    let n = ninputs - (AeInst.n_in i) + (AeInst.n_out i) in
    (* only loop if 'i' is a valid instruction to use here *)
    if n >= 0 
    && AeInst.n_in i <= ninputs
    && not (AeStack.is_pruneable i block) then
      loop (depth - 1) n (i :: block)
  and loop depth ninputs block =
    match depth with
    | 0 ->
      let block = List.rev block in
      if ninputs = 0 && AeStack.is_valid block then
        process block
    | _ when depth > 0 ->
      List.iter insts (iter depth ninputs block)
    | _ -> ()
  in
  List.iter insts (iter depth 0 []);
  !blocks
