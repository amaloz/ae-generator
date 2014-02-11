open Core.Std
open MoOps
module MoInst = MoInstructions

let create_dup_tbl () =
  let module Key = struct
    module T = struct
      type t = string list with sexp
      let compare = List.compare ~cmp:String.compare
      let hash = Hashtbl.hash
    end
    include Hashtbl.Make(T)
  end in
  Key.create ()

let exists tbl g =
  let r = MoGraph.eval g in
  match Hashtbl.find tbl r with
  | Some _ -> true
  | None -> Hashtbl.set tbl ~key:r ~data:true; false

let is_valid g = MoGraph.is_valid g
let is_decryptable g = is_valid g && MoGraph.is_decryptable g
let is_secure g = MoGraph.check g

let gen f init depth insts =
  let blocks = ref [] in
  let unprocessed = ref [] in
  let tbl = create_dup_tbl () in
  let process init block =
    Log.infof "Trying [%s] [%s]"
              (MoInst.string_of_t_list init)
              (MoInst.string_of_t_list block);
    let g = MoGraph.create init block in
    try
      if f g then
        begin
          Log.infof "It works!";
          if exists tbl g then
            Log.infof "already exists..."
          else
            blocks := block :: !blocks
        end
    with _ ->
      Log.warnf "bizarre error encountered!";
      unprocessed := List.append !unprocessed [block]
  in
  let rec iter depth ninputs block i =
    let n = ninputs - (MoInst.n_in i) + (MoInst.n_out i) in
    if MoInst.n_in i <= ninputs
       && n >= 0
       && not (MoStack.is_pruneable i block) then
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
  List.iter insts (iter (depth - 1) 1 [Instruction Start]);
  (* XXX: total hack.  Because some seemingly random graphs just don't parse
  correctly, we store those blocks in the "unprocessed" list, and then
  repeatedly iterative over it until we have no more failures. *)
  let rec f () =
    Log.infof "RETRYING %d BLOCK(S)" (List.length !unprocessed);
    let l = !unprocessed in
    let old_length = List.length l in
    unprocessed := [];
    List.iter l (process init);
    let len = List.length !unprocessed in
    if len <> 0 && len <> old_length then
      f ()
    else if len <> old_length then
      Log.errorf "Giving up on processing block!"
  in
  f ();
  !blocks
