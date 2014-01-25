open Core.Std
open MoOps
module MoInst = MoInstructions

let create_dup_tbl () =
  let module Key = struct
    module T = struct
      type t = string with sexp
      let compare = String.compare
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

let gengraphs init depth insts =
  let blocks = ref [] in
  let tbl = create_dup_tbl () in
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
             Log.errorf "FUCK! Skipping...";
         end
    | _ ->
       List.iter insts (iter depth ninputs block)
  in
  List.iter insts (iter depth 1 []);
  
  !blocks
