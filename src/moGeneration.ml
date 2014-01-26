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
  (* Log.debugf "%s\n%!" (List.to_string ~f:ident r); *)
  match Hashtbl.find tbl r with
  | Some _ -> true
  | None -> Hashtbl.set tbl ~key:r ~data:true; false

let gengraphs init depth insts =
  let blocks = ref [] in
  let unprocessed = ref [] in
  let tbl = create_dup_tbl () in
  let process init block =
    if MoUtils.get_debug_level () = 2 then
      Log.infof "Trying [%s] [%s]"
                (MoInst.string_of_t_list init)
                (MoInst.string_of_t_list block);
    let g = MoGraph.create init block in
    try
      if MoGraph.check g then
        begin
          Log.infof "It works!";
          if exists tbl g then
            Log.infof "already exists..."
          else
            blocks := block :: !blocks
        end
    with _ ->
      Log.errorf "bizarre error encountered!";
      unprocessed := List.append !unprocessed [block]
      (* unprocessed := block :: !unprocessed *)
  in
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
         process init block
    | _ ->
       List.iter insts (iter depth ninputs block)
  in
  List.iter insts (iter (depth - 1) 1 [Instruction Start]);
  (* XXX: total hack.  Because random graphs just don't parse correctly, we
  store those blocks in the "unprocessed" list, and then repeatedly iterative
  over it until we have no more failures. *)
  let rec f () =
    Log.infof "RETRYING %d BLOCK(S)" (List.length !unprocessed);
    let l = !unprocessed in
    unprocessed := [];
    List.iter l (process init);
    if List.length !unprocessed <> 0 then
      f ()
  in
  f ();
  !blocks
