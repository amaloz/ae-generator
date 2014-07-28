open Core.Std
open MoOps
module MoInst = MoInstructions

module E = struct
  type t = Int.Set.t
  let compare = Int.Set.compare
  let default = Int.Set.empty
end
module V = struct type t = MoOps.instruction end
module G = Graph.Imperative.Digraph.AbstractLabeled(V)(E)

let string_of_e e =
  let l = G.E.label e |> Int.Set.to_list in
  List.to_string Int.to_string l

(* Maintain separate vertex and edge orderings so we can correctly traverse in
order. We also need a pointer to the out node in block, as this is needed in the
decryptability check. *)
type t = { g : G.t; v : G.V.t list; e : G.E.t list; out : G.V.t }

let create init block =
  let s = Stack.create () in
  let g = G.create () in
  let vl = ref [] in
  let el = ref [] in
  let f inst =
    match inst with
    | Instruction i ->
       let dst = G.V.create i in
       G.add_vertex g dst;
       vl := List.append !vl [dst];
       for j = 1 to MoInst.n_in inst do
         let src = Stack.pop_exn s in
         let e = G.E.create src Int.Set.empty dst in
         G.add_edge_e g e;
         el := List.append !el [e];
       done;
       for j = 1 to MoInst.n_out inst do
         Stack.push s dst
       done
    | StackInstruction i ->
       begin
         match i with
         | Swap ->
            let first = Stack.pop_exn s in
            let second = Stack.pop_exn s in
            Stack.push s first;
            Stack.push s second
         | Twoswap ->
            let first = Stack.pop_exn s in
            let second = Stack.pop_exn s in
            let third = Stack.pop_exn s in
            Stack.push s first;
            Stack.push s second;
            Stack.push s third
       end
  in
  List.iter init f;
  List.iter block f;
  let out = List.find_exn (List.rev !vl) ~f:(fun v -> G.V.label v = Out) in
  let t = { g = g; v = !vl; e = !el; out = out } in
  t

let display_with_feh t =
  let module Display = struct
    include G
    let ctr = ref 1
    let vertex_name v =
      let c =
        if Mark.get v = 0
        then begin
          Mark.set v !ctr;
          ctr := !ctr + 1;
          Mark.get v
        end
        else Mark.get v in
      Printf.sprintf "%s_%d" (MoInst.string_of_t (Instruction (V.label v))) c
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes e = [`Label (string_of_e e)]
    let get_subgraph _ = None
  end in
  let module Dot = Graph.Graphviz.Dot(Display) in
  let tmp = Filename.temp_file "mode" ".dot" in
  G.Mark.clear t.g;
  let oc = Out_channel.create tmp in
  Dot.output_graph oc t.g;
  Out_channel.close oc;
  ignore (Sys.command ("dot -Tpng " ^ tmp ^ " | feh -"));
  Sys.remove tmp

exception AssignFamiliesException

let assign_families t =
  Log.infof "Assigning families to graph...";
  let replace_edge e set =
    G.remove_edge_e t.g e;
    let e = G.E.create (G.E.src e) set (G.E.dst e) in
    Log.debugf "    Families = %s" (string_of_e e);
    G.add_edge_e t.g e
  in
  let parents e = G.E.src e |> G.pred_e t.g in
  let parent e = parents e |> List.hd_exn in
  let fam_cnt = ref 0 in
  let f e =
    let label = G.E.src e |> G.V.label in
    Log.debugf "  Hit edge %s -> %s"
               ((MoOps.Instruction label) |> MoInst.string_of_t)
               ((MoOps.Instruction (G.E.dst e |> G.V.label)) |> MoInst.string_of_t);
    match label with
    | Dup
    | Inc
    | Nextiv_init ->
       let e' = parent e in
       replace_edge e (G.E.label e')
    | Genrand
    | M
    | Prf
    | Prp
    | Start ->
       let set = Int.Set.singleton !fam_cnt in
       fam_cnt := !fam_cnt + 1;
       replace_edge e set
    | Xor ->
       let elist = parents e in
       let l = List.hd_exn elist in
       let r = List.last_exn elist in
       let inter = Int.Set.inter (G.E.label l) (G.E.label r) in
       if Int.Set.length inter <> 0 then
         raise AssignFamiliesException;
       let fam = Int.Set.union (G.E.label l) (G.E.label r) in
       replace_edge e fam
    | Nextiv_block
    | Out ->
       failwith "should not reach here!"
  in
  List.iter t.e f;
  t

let validate t =
  Log.infof "Validating graph...";
  let smt = MoSmt.create () in
  let f v =
    Log.debugf "  Hit %s" ((MoOps.Instruction (G.V.label v)) |> MoInst.string_of_t);
    MoSmt.op smt (G.V.label v) in
  List.iter t.v f;
  MoSmt.finalize smt;
  let fname = Filename.temp_file "z3" ".smt2" in
  MoSmt.write_to_file smt fname;
  let result = MoSmt.run fname in
  Sys.remove fname;
  result

type dir = Forward | Backward
let string_of_dir = function
  | Forward -> "Forward"
  | Backward -> "Backward"

let is_decryptable t =
  (* find label 'label' in graph *)
  let find label =
    let f x y = if (G.V.label x) = label then x :: y else y in
    let l = G.fold_vertex f t.g [] in
    assert (List.length l = 1);
    List.hd_exn l
  in
  let nextiv_block = find Nextiv_block in
  let nextiv_init = find Nextiv_init in
  let out = t.out in
  let rec loop cur prev dir want_out =
    Log.debugf "cur = %s | prev = %s | dir = %s"
               (MoInst.string_of_t (Instruction (G.V.label cur)))
               (MoInst.string_of_t (Instruction (G.V.label prev)))
               (string_of_dir dir);
    let next cur prev dir =
      let l = match dir with
        | Forward -> G.succ t.g cur
        | Backward -> G.pred t.g cur in
      List.filter l ~f:(fun v -> v <> prev)
    in
    let continue cur dir =
      let l = next cur prev dir in
      match List.hd l with
      | Some v -> loop v cur dir want_out
      | None -> false
    in
    (* stop if we're in a cycle *)
    if G.Mark.get cur <> 0 then false
    else
      begin
        G.Mark.set cur 1;
        match G.V.label cur with
        | Dup ->
           begin
             match dir with
             | Forward ->
                let l = next cur prev dir in
                List.exists l (fun v -> loop v cur dir want_out)
             | Backward ->
                begin
                  let n = List.hd (next cur prev Forward) in
                  let p = List.hd (next cur prev Backward) in
                  match n, p with
                  | Some n, Some p ->
                     loop n cur Forward want_out || loop p cur Backward want_out
                  | Some _, None -> failwith "Fatal: DUP hit with no parent?"
                  | None, Some _ ->
                     (* this case can happen if DUP connects directly to XOR, in
                     which case we have *one* child, but as that child is the
                     node we are coming from, the 'next' function returns
                     nothing *)
                     false
                  | None, None -> failwith "Fatal: DUP hit with no edges?"
                end
           end
        | Inc | Prp -> continue cur dir
        | Genrand -> false
        | M -> not want_out
        | Nextiv_block ->
           begin
             match dir with
             | Forward -> true
             | Backward -> continue cur dir
           end
        | Nextiv_init ->
           begin
             match dir with
             | Forward -> failwith "Fatal: NEXTIV in init hit going forward?"
             | Backward -> continue cur dir
           end
        | Out ->
           begin
             match dir with
             | Forward -> want_out
             | Backward -> continue cur dir
           end
        | Start -> true
        | Prf ->
           begin
             match dir with
             | Forward -> continue cur dir
             | Backward -> false
           end
        | Xor ->
           begin
             match dir with
             | Forward ->
                begin
                  let v = List.hd (next cur prev Forward) in
                  let v' = List.hd (next cur prev Backward) in
                  match v, v' with
                  | Some v, Some v' ->
                     loop v cur Forward want_out
                     || loop v' cur Backward want_out
                  | Some _, None -> false
                  | None, Some _ -> failwith "Fatal: XOR hit with no child?"
                  | None, None -> failwith "Fatal: XOR hit with no neighbors?"
                end
             | Backward ->
                let l = next cur prev Backward in
                List.length l = 2
                && List.for_all l ~f:(fun v -> loop v cur Backward want_out)
           end
      end
  in
  let startloop node dir want_out =
    (* reset graph marks before traversing *)
    G.Mark.clear t.g;
    loop node node dir want_out
  in
  let r = startloop out Backward false
          && startloop nextiv_init Backward true
          && startloop nextiv_block Backward true
  in
  if not r then
    Log.infof "Is not decryptable!";
  r

exception Vertex of G.V.t

let is_connected t =
  let module H = Caml.Hashtbl.Make(G.V) in
  let h = H.create 65537 in
  let rec visit s =
    match Stack.pop s with
      | None -> ()
      | Some v ->
        if not (H.mem h v) then begin
          H.add h v ();
          G.iter_succ (Stack.push s) t.g v;
          G.iter_pred (Stack.push s) t.g v
        end;
        visit s
  in
  let s = Stack.create () in
  let v =
    try
      List.iter t.v (fun v -> raise (Vertex v));
      raise Not_found
    with Vertex v -> v
  in
  Stack.push s v;
  visit s;
  try
    let f v = if not (H.mem h v) then raise Exit in
    List.iter t.v f;
    true
  with Exit ->
    Log.infof "Is not connected!";
    false

let has_right_nodes t =
  let prp_or_prf =
    (List.exists t.v (fun v -> G.V.label v = Prp))
    || (List.exists t.v (fun v -> G.V.label v = Prf))
  in
  let has inst num = List.count t.v (fun v -> G.V.label v = inst) = num in
  prp_or_prf
  && has M 1
  && has Start 1
  && has Out 2                  (* one in Init and Block each *)
  && has Nextiv_init 1
  && has Nextiv_block 1
  && has Genrand 1              (* one in Init, none in Block *)

let is_start_location_valid t =
  let f e =
    if G.E.src e |> G.V.label = Nextiv_init
       && G.E.dst e |> G.V.label <> Start then
      begin
        Log.infof "start location invalid!";
        raise Exit
      end
  in
  try List.iter t.e f; true with Exit -> false

let is_valid t = is_start_location_valid t && has_right_nodes t && is_connected t

let is_secure t =
  try
    assign_families t |> validate
  with AssignFamiliesException -> false
