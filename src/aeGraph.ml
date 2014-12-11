open Core.Std
open AeOps
open AeInst

type typ =
  | Zero
  | One
  | Bot
  | Rand

let string_of_typ = function
  | Zero -> "0"
  | One -> "1"
  | Bot -> "⊥"
  | Rand -> "$"

let xor_types a b =
  match a, b with
  | Zero, Zero | One, One -> Zero
  | Zero, One | One, Zero -> One
  | _, _ -> failwith "cannot xor other types"

type map = { typ : typ; ctr : int }

module V = struct
  type t = AeOps.instruction * map option ref
end
module G = Graph.Imperative.Digraph.Abstract(V)
module Topo = Graph.Topological.Make(G)

let string_of_v v =
  let inst, _ = G.V.label v in
  AeInst.string_of_t (Instruction inst)

let full_string_of_v v =
  let inst, map = G.V.label v in
  let l = match !map with
    | Some map ->
      [string_of_typ map.typ; Int.to_string map.ctr]
    | None -> []
  in
  List.append [string_of_v v] l |> String.concat ~sep:" " 

type t = { g : G.t; phase : phase; starts : G.V.t list; checks : G.V.t list }

let create block phase =
  let s = Stack.create () in
  let g = G.create () in
  let checks = ref [] in
  let starts = ref [] in
  let f inst =
    match inst with
    | Instruction i ->
      let dst = G.V.create (i, ref None) in
      G.add_vertex g dst;
      for j = 1 to AeInst.n_in inst do
        let src = Stack.pop_exn s in
        let e = G.E.create src () dst in
        G.add_edge_e g e;
      done;
      for j = 1 to AeInst.n_out inst do
        Stack.push s dst
      done;
      (* Construct list of starting locations *)
      begin
        match i with
        | Ini | Msg -> starts := !starts @ [dst]
        | _ -> ()
      end;
      (* Construct list of nodes to check *)
      begin
        match phase with
        | Encode -> begin
            match i with
            | Out -> checks := dst :: !checks
            | _ -> ()
          end
        | Decode -> begin
            match i with
            | Fin -> if List.is_empty !checks then checks := [dst]
            | _ -> ()
          end
        | Tag -> begin
            match i with
            | Out -> if List.is_empty !checks then checks := [dst]
            | _ -> ()
          end
      end
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
  List.iter block f;
  { g = g; phase = phase; starts = !starts; checks = !checks }

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
        else Mark.get v
      in
      Printf.sprintf "%s_%d" (string_of_v v) c
    let graph_attributes _ = []
    let default_vertex_attributes _ = []
    let vertex_attributes _ = []
    let default_edge_attributes _ = []
    let edge_attributes _ = []
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

let derive_encode_graph t =
  if t.phase <> Decode then
    failwith "input must be decode graph";
  { g = G.create (); phase = Encode; starts = []; checks = [] }

let clear t =
  let f v =
    let _, map = G.V.label v in
    map := None
  in
  Topo.iter f t.g

let check t types rand checks =
  Log.infof "Checking %s..." (List.to_string string_of_typ types);
  let max_ctr = ref 0 in
  let s = Stack.of_list types in
  clear t;
  let f v typ =
    let inst, map = G.V.label v in
    let f = function
      | Rand -> max_ctr := 1; 1
      | _ -> 0 in
    let typ = Stack.pop_exn s in
    map := Some { typ = typ; ctr = f typ }
  in
  List.iter2_exn t.starts types f;
  let f v =
    let inst, _ = G.V.label v in
    match inst with
    | Msg | Ini ->
      Log.debugf "%s" (full_string_of_v v)
    | Fin | Out | Dup ->
      let p = G.pred t.g v |> List.hd_exn in
      let _, pmap = G.V.label p in
      let _, map = G.V.label v in
      map := !pmap;
      Log.debugf "%s" (full_string_of_v v)
    | Tbc ->
      let p = G.pred t.g v |> List.hd_exn in
      let _, pmap = G.V.label p in
      let _, map = G.V.label v in
      let pmap = Option.value_exn !pmap in
      if pmap.typ = One || pmap.typ = Rand || rand then
        begin
          max_ctr := !max_ctr + 1;
          map := Some { typ = Rand; ctr = !max_ctr }
        end
      else
        map := Some pmap;
      Log.debugf "%s" (full_string_of_v v)
    | Xor ->
      let ps = G.pred t.g v in
      let p1 = List.nth_exn ps 0 in
      let p2 = List.nth_exn ps 1 in
      let _, p1map = G.V.label p1 in
      let _, p2map = G.V.label p2 in
      let p1map = Option.value_exn !p1map in
      let p2map = Option.value_exn !p2map in
      let p1map, p2map =
        if p1map.ctr < p2map.ctr then
          p2map, p1map
        else
          p1map, p2map
      in
      let _, map = G.V.label v in
      if (p1map.typ = Zero && p2map.typ = Zero)
      || (p1map.typ = Zero && p2map.typ = One)
      || (p1map.typ = One && p2map.typ = Zero) then
        map := Some { typ = xor_types p1map.typ p2map.typ; ctr = p1map.ctr }
      else if p1map.typ = Rand && p1map.ctr > p2map.ctr then
        map := Some { typ = Rand; ctr = p1map.ctr }
      else
        map := Some { typ = Bot; ctr = p1map.ctr };
      Log.debugf "%s" (full_string_of_v v)
  in
  Topo.iter f t.g;
  (* Check that all nodes needing to be random are indeed marked random *)
  let f check =
    let _, map = G.V.label check in
    match !map with
    | Some map -> map.typ = Rand
    | None -> false
  in
  List.for_all checks f

let is_secure_decode t =
  check t [Zero; Zero; One; Zero] false t.checks
  && check t [Zero; Zero; Zero; One] false t.checks
  && check t [Zero; Zero; One; One] false t.checks
    
  && check t [Rand; Zero; Zero; Zero] false t.checks
  && check t [Rand; Zero; Zero; One] false t.checks
  && check t [Rand; Zero; One; Zero] false t.checks
  && check t [Rand; Zero; One; One] false t.checks

  && check t [Rand; One; Zero; Zero] false t.checks
  && check t [Rand; One; Zero; One] false t.checks
  && check t [Rand; One; One; Zero] false t.checks
  && check t [Rand; One; One; One] false t.checks

let is_secure_tag t =
  check t [Bot; Bot] true t.checks
  && check t [Rand; Zero] true t.checks
  && check t [Rand; One] true t.checks

let is_secure t =
  match t.phase with
  | Encode -> failwith "not implemented yet"
  | Decode -> is_secure_decode t
  | Tag -> is_secure_tag t
