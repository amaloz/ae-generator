open Core.Std
open MoOps

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

let exists tbl (init, block) =
  let r = MoStack.eval init block in
  match Hashtbl.find tbl r with
    | Some _ -> true
    | None ->
      Hashtbl.set tbl ~key:r ~data:true;
      false

let cmp (init, block) (init', block') =
  let is_prf i = i = Instruction Prf in
  let is_prp i = i = Instruction Prp in
  let is_tprp i = i = Instruction TPrp in
  let count i = is_prf i || is_prp i || is_tprp i in
  let n = List.count block ~f:count in
  let n' = List.count block' ~f:count in
  if n < n' then -1
  else if n = n' then 0
  else 1
