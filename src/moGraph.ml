open Core.Std
open MoOps
module MoInst = MoInstructions

type int_or_string =
  | Int of int
  | Str of string

type typename = R | U | B
             
type edgelabel =
    {
      family : int;
      etype : typename;
      outflag : bool;
      prfflag : bool;
      incflag : bool;
    }
             
(* module E = struct *)
(*   type t = int_or_string *)
(*   let compare x y = *)
(*     match x, y with *)
(*       | Int x, Int y -> Int.compare x y *)
(*       | Str x, Str y -> String.compare x y *)
(*       | _, _ -> failwith "invalid compare" *)
(*   let default = Int 0 *)
(* end *)
module E = struct
  type t = edgelabel
end
module V = struct type t = MoOps.instruction end
module G = Graph.Imperative.Digraph.AbstractLabeled(V)(E)

let string_of_e e =
  match G.E.label e with
    | Int x -> string_of_int x
    | Str x -> x

type t = G.t

let create init block =
  let s = Stack.create () in
  let g = G.create () in
  let rec f inst =
    match inst with
      | Instruction i ->
        let v = G.V.create i in
        G.add_vertex g v;
        for j = 1 to MoInst.n_in inst do
          let e = G.E.create (Stack.pop_exn s) (Int j) v in
          G.add_edge_e g e
        done;
        for j = 1 to MoInst.n_out inst do
          Stack.push s v
        done
      | StackInstruction i ->
        MoInst.mod_stack i s
      | Subroutine (_, block, _, _) ->
        List.iter block f
  in
  List.iter init f;
  List.iter block f;
  g

(* let is_nextiv_value_changed t = *)
(*   (\* XXX: This code is a total hack! *\) *)
(*   let f v = *)
(*     if G.V.label v = Nextiv_init *)
(*     then begin *)
(*       let p = List.hd_exn (G.pred t v) in *)
(*       if G.V.label p = Genzero *)
(*       then () *)
(*       else begin *)
(*         let n = List.hd_exn (G.succ t v) in *)
(*         if G.V.label n = Dup *)
(*         then begin *)
(*           let l = G.succ t n in *)
(*           if List.exists l ~f:(fun v -> G.V.label v = Nextiv_block) *)
(*           then raise Not_found *)
(*           else () *)
(*         end *)
(*       end *)
(*     end *)
(*   in *)
(*   try G.iter_vertex f t; true *)
(*   with Not_found -> false *)

(* type dir = Forward | Backward *)
(* let string_of_dir = function *)
(*   | Forward -> "Forward" *)
(*   | Backward -> "Backward" *)

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
    else begin
      G.Mark.set cur 1;
      match G.V.label cur with
        | Double -> failwith "DBL not done yet"
        | Dup -> begin
          match dir with
            | Forward ->
              let l = next cur prev dir in
              let f v = loop g v cur dir in
              List.exists l f
            | Backward -> begin
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
        | Genzero -> true
        | Inc -> continue cur dir
        | M -> continue cur dir
        | Nextiv_init -> continue cur dir
        | Nextiv_block -> false
        | Out -> true
        | Prf -> begin
          match dir with
            | Forward -> false
            | Backward -> continue cur dir
        end
        | Prp -> continue cur dir
        | TPrp -> begin
          let is_e_tweak p =
            let e = G.find_edge g p cur in
            G.E.label e = Int 1 in
          match dir with
            | Forward -> begin
              let n = List.hd_exn (next cur prev Forward) in
              let l = next cur prev Backward in
              match List.find l ~f:(is_e_tweak) with
                | Some p ->
                  (* try and find the tweak and the next item *)
                  loop g p cur Backward && loop g n cur Forward
                | None ->
                  (* we came from the tweak, thus there's no way we can learn
                     the TPRP input *)
                  false
            end
            | Backward -> begin
              (* if we can find the tweak, we can decrypt *)
              let l = next cur prev Backward in
              match List.find l ~f:(is_e_tweak) with
                | Some p -> loop g p cur Backward
                | None -> failwith "Fatal: no tweak in TPRP input?"
            end
        end
        | Xor -> begin
          match dir with
            | Forward -> begin
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
  List.for_all (find_ms t) (fun v -> loop t v v Forward)

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
  with Exit -> false

let is_pruneable t =
  let f v =
    let l = G.succ t v in
    let f v' =
      match G.V.label v, G.V.label v' with
        | Dup, Dup
        | Genrand, (Out | Nextiv_init | Nextiv_block)
        | Genzero, Out
        | Inc, (Inc | Out)
        | M, (Inc | Out)
        | Prp, Prp -> raise Exit
        | M, Dup ->
          let l = G.succ t v' in
          if List.exists l (fun x -> G.V.label x = Out)
          then raise Exit
          else ()
        | _, _ -> ()
    in
    List.iter l f
  in
  try G.iter_vertex f t; false
  with Exit -> true

let count_inst t i =
  let f v cnt = if G.V.label v = i then cnt + 1 else cnt in
  G.fold_vertex f t 0

let check init block =
  let t = create init block in
  if not (is_pruneable t)
    && is_nextiv_value_changed t
    && is_connected t
    && is_decryptable t
  then true
  else false

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
  let f v v' l =
    match l with
      | [] -> []
      | (_, tag) :: rest ->
        let e = G.E.create v (Str tag) v' in
        G.remove_edge t v v';
        G.add_edge_e t e;
        rest
  in
  let r = G.fold_edges f t l in
  assert (List.length r = 0);
  display_with_feh t
