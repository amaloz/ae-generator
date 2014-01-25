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

type t = G.t

let create init block =
  let s = Stack.create () in
  let g = G.create () in
  let f inst =
    match inst with
    | Instruction i ->
       let dst = G.V.create i in
       G.add_vertex g dst;
       for j = 1 to MoInst.n_in inst do
         let src = Stack.pop_exn s in
         let e = G.E.create src Int.Set.empty dst in
         G.add_edge_e g e
       done;
       for j = 1 to MoInst.n_out inst do
         Stack.push s dst
       done
    | StackInstruction i ->
       begin
         match i with
         | Swap ->
            let x = Stack.pop_exn s in
            let x' = Stack.pop_exn s in
            Stack.push s x;
            Stack.push s x'
         | Twoswap ->
            let x = Stack.pop_exn s in
            let x' = Stack.pop_exn s in
            let x'' = Stack.pop_exn s in
            Stack.push s x;
            Stack.push s x';
            Stack.push s x''
       end
  in
  List.iter init f;
  List.iter block f;
  g

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
  G.Mark.clear t;
  let oc = Out_channel.create tmp in
  Dot.output_graph oc t;
  Out_channel.close oc;
  ignore (Sys.command ("dot -Tpng " ^ tmp ^ " | feh -"));
  Sys.remove tmp

let display_model_with_feh t l =
  failwith "display_model_with_feh not done yet!"
  (* let f v v' l = *)
  (*   match l with *)
  (*   | [] -> [] *)
  (*   | (_, tag) :: rest -> *)
  (*      let e = G.E.create v (Str tag) v' in *)
  (*      G.remove_edge t v v'; *)
  (*      G.add_edge_e t e; *)
  (*      rest *)
  (* in *)
  (* let r = G.fold_edges f t l in *)
  (* assert (List.length r = 0); *)
  (* display_with_feh t *)

exception AssignFamiliesException

let assign_families t =
  Log.infof "Assigning families to graph...";
  let replace_edge e set =
    G.remove_edge_e t e;
    let e = G.E.create (G.E.src e) set (G.E.dst e) in
    Log.debugf "    Families = %s" (string_of_e e);
    G.add_edge_e t e
  in
  let parents e = G.E.src e |> G.pred_e t in
  let parent e = parents e |> List.hd_exn in
  let are_parents_processed e =
    let f e = if Int.Set.length (G.E.label e) = 0 then raise Exit in
    try List.iter (parents e) f; true with Exit -> false
  in
  let fam_cnt = ref 0 in
  let f e =
    let label = G.E.src e |> G.V.label in
    Log.debugf "  Hit edge %s -> %s"
               ((MoOps.Instruction label) |> MoInst.string_of_t)
               ((MoOps.Instruction (G.E.dst e |> G.V.label)) |> MoInst.string_of_t);
    if are_parents_processed e then
      begin
        match label with
        | Dup
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
      end
    else
      begin
        display_with_feh t;
        failwith "WHY!!!!!!!!!!!!!!!!"
      end
  in
  G.iter_edges_e f t;
  t

let validate ?(save=None) ?(model=None) t =
  Log.infof "Validating graph...";
  let smt = MoSmt.create () in
  let f v =
    Log.debugf "  Hit %s" ((MoOps.Instruction (G.V.label v)) |> MoInst.string_of_t);
    MoSmt.op smt (G.V.label v) in
  G.iter_vertex f t;
  MoSmt.finalize smt;
  let fname = match save with
    | Some fn -> fn
    | None -> Filename.temp_file "z3" ".smt2" in
  MoSmt.write_to_file smt fname;
  let result = MoSmt.run fname in
  if Option.is_none save then
    Sys.remove fname;
  result

type dir = Forward | Backward

(* XXX: PCBC fails here *)
let is_decryptable t =
  let rec loop g cur prev dir =

    (* Printf.printf "cur = %s | prev = %s | dir = %s\n%!" *)
    (*   (MoInst.string_of_t (Instruction (G.V.label cur))) *)
    (*   (MoInst.string_of_t (Instruction (G.V.label prev))) *)
    (*   (string_of_dir dir); *)

    let next cur prev dir =
      let l = match dir with
        | Forward -> G.succ g cur
        | Backward -> G.pred g cur in
      List.filter l ~f:(fun v -> v <> prev) in

    let continue cur dir =
      let l = next cur prev dir in
      match List.hd l with
      | Some v -> loop g v cur dir
      | None -> false in

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
                let f v = loop g v cur dir in
                List.exists l f
             | Backward ->
                begin
                  let n = List.hd (next cur prev Forward) in
                  let p = List.hd (next cur prev Backward) in
                  match n, p with
                  | Some n, Some p ->
                     loop g n cur Forward || loop g p cur Backward
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
        | Genrand -> false
        (* | Inc -> continue cur dir *)
        | M -> continue cur dir
        | Nextiv_init
        | Start -> continue cur dir
        | Nextiv_block -> false
        | Out -> true
        | Prf ->
           begin
             match dir with
             | Forward -> false
             | Backward -> continue cur dir
           end
        | Prp -> continue cur dir
        | Xor ->
           begin
             match dir with
             | Forward ->
                begin
                  let v = List.hd (next cur prev Forward) in
                  let v' = List.hd (next cur prev Backward) in
                  match v, v' with
                  | Some v, Some v' ->
                     (loop g v cur Forward) && (loop g v' cur Backward)
                  | Some _, None -> false
                  | None, Some _ -> failwith "Fatal: XOR hit with no child?"
                  | None, None -> failwith "Fatal: XOR hit with no neighbors?"
                end
             | Backward ->
                let l = next cur prev Backward in
                let f v = loop g v cur Backward in
                List.length l = 2 && List.for_all l ~f:f
           end
      end
  in
  (* reset graph marks before traversing *)
  G.Mark.clear t;
  let find_ms g =
    let f x y = if (G.V.label x) = M then x :: y else y in
    G.fold_vertex f g [] in
  let r = List.for_all (find_ms t) (fun v -> loop t v v Forward) in
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
          G.iter_succ (Stack.push s) t v;
          G.iter_pred (Stack.push s) t v
        end;
        visit s
  in
  let s = Stack.create () in
  let v =
    try
      G.iter_vertex (fun v -> raise (Vertex v)) t;
      raise Not_found
    with Vertex v -> v
  in
  Stack.push s v;
  visit s;
  try
    let f v = if not (H.mem h v) then raise Exit in
    G.iter_vertex f t;
    true
  with Exit ->
    Log.infof "Is not connected!";
    false

(* let is_pruneable t = *)
(*   let f v = *)
(*     let l = G.succ t v in *)
(*     let f v' = *)
(*       match G.V.label v, G.V.label v' with *)
(*         | Dup, Dup *)
(*         | Genrand, (Out | Nextiv_init | Nextiv_block) *)
(*         | Genzero, Out *)
(*         | Inc, (Inc | Out) *)
(*         | M, (Inc | Out) *)
(*         | Prp, Prp -> raise Exit *)
(*         | M, Dup -> *)
(*           let l = G.succ t v' in *)
(*           if List.exists l (fun x -> G.V.label x = Out) *)
(*           then raise Exit *)
(*           else () *)
(*         | _, _ -> () *)
(*     in *)
(*     List.iter l f *)
(*   in *)
(*   try G.iter_vertex f t; false *)
(*   with Exit -> true *)

(* let count_inst t i = *)
(*   let f v cnt = if G.V.label v = i then cnt + 1 else cnt in *)
(*   G.fold_vertex f t 0 *)

let is_start_location_valid t =
  let f e =
    if G.E.src e |> G.V.label = Nextiv_init
       && G.E.dst e |> G.V.label <> Start then
      begin
        Log.infof "start location invalid!";
        raise Exit
      end
  in
  try G.iter_edges_e f t; true with _ -> false

let check ?(save=None) ?(model=None) t =
  if is_start_location_valid t
     && is_connected t
     && is_decryptable t
  then
    let t = assign_families t in
    validate ~save:save ~model:model t
  else
    false

let eval t =
  let hex s = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) s in
  let tohex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s in
  let chr = function
    | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5'
    | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9' | 10 -> 'a' | 11 -> 'b'
    | 12 -> 'c' | 13 -> 'd' | 14 -> 'e' | 15 -> 'f'
    | _ -> failwith "Fatal: invalid character"
  in
  let ord = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
    | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'a' -> 10 | 'b' -> 11
    | 'c' -> 12 | 'd' -> 13 | 'e' -> 14 | 'f' -> 15
    | _ -> failwith "Fatal: invalid character"
  in
  let xor s s' =
    assert ((String.length s) = (String.length s'));
    let len = String.length s in
    let xor c c' = chr ((ord c) lxor (ord c')) in
    let r = String.create len in
    for i = 0 to len - 1 do
      r.[i] <- xor s.[i] s'.[i]
    done;
    assert ((String.length r) = (String.length s));
    r
  in
  let out = ref "" in
  let msg = "12345678123456781234567812345678" in
  let key = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
  let rnd = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
  let c = new Cryptokit.Block.aes_encrypt (hex key) in
  let s = Stack.create () in
  let visit v =
    match G.V.label v with
    | Dup ->
       let r = String.copy (Stack.top_exn s) in
       Stack.push s r
    | Genrand ->
       Stack.push s (hex rnd)
    | M ->
       Stack.push s (hex msg)
    | Nextiv_init
    | Start ->
       ()
    | Nextiv_block ->
       let _ = Stack.pop_exn s in
       ()
    | Out ->
       out := tohex (Stack.pop_exn s)
    | Prf ->
       let h = Cryptokit.Hash.md5 () in
       let r = Cryptokit.hash_string h (Stack.pop_exn s) in
       Stack.push s r
    | Prp ->
       let r = String.create 16 in
       c#transform (Stack.pop_exn s) 0 r 0;
       Stack.push s r
    | Xor ->
       let s1 = Stack.pop_exn s in
       let s2 = Stack.pop_exn s in
       let r = xor (tohex s1) (tohex s2) in
       Stack.push s (hex r)
  in
  G.iter_vertex visit t;
  !out
