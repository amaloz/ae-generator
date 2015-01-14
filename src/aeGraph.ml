open Core.Std
open Async.Std
open AeOps

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
  | _ , _ -> assert false

type map = { typ : typ; ctr : int }

module V = struct
  (* Each vertex contains three elements: the instruction the vertex represents,
     a map reference for checking security of the graph, and a string reference
     for evaluating the graph.  *)
  (* XXX: Can we do this without storing the map ref and string ref within the
     vertex type? *)
  type t = inst * map ref * string ref
end
module G = Graph.Imperative.Digraph.Abstract(V)
module Topo = Graph.Topological.Make(G)
module Weight = struct
  type label = G.E.label
  type t = int
  let weight _ = Int.one
  let compare = Int.compare
  let add = Int.(+)
  let zero = Int.zero
end
module Dijkstra = Graph.Path.Dijkstra(G)(Weight)

let string_of_v v =
  let inst, _, _ = G.V.label v in
  string_of_inst inst

let string_of_e e =
  let src, _, _ = G.E.src e |> G.V.label in
  let dst, _, _ = G.E.dst e |> G.V.label in
  [string_of_inst src; string_of_inst dst]
  |> String.concat ~sep:" -> "

let full_string_of_v v =
  let _, map, _ = G.V.label v in
  [string_of_typ !map.typ; Int.to_string !map.ctr]
  |> List.append [string_of_v v] |> String.concat ~sep:" "

let find_vertex_by_inst g inst =
  let f v r =
    match r with
    | Some _ -> r
    | None ->
      let inst', _, _ = G.V.label v in
      if inst' = inst then Some v else None
  in
  Option.value_exn (G.fold_vertex f g None)

type t = { g : G.t; phase : phase; checks : G.V.t list }

exception Stack_error of string

let create block phase =
  let s = Stack.create () in
  let g = G.create () in
  let errmsg op = sprintf "%s: Not enough items on stack" (string_of_op op) in
  let f acc op =
    match op with
    | Inst i -> begin
        let dst = G.V.create (i, ref { typ = Bot; ctr = 0 }, ref "") in
        G.add_vertex g dst;
        for j = 1 to AeInst.n_in op do
          match Stack.pop s with
          | Some src ->
            let e = G.E.create src () dst in
            G.add_edge_e g e;
          | None -> raise (Stack_error (errmsg op))
        done;
        for j = 1 to AeInst.n_out op do
          Stack.push s dst
        done;
        (* Construct list of nodes to check *)
        match phase with
        | Encode ->
          if i = Out1 || i = Out2 then dst :: acc else acc
        | Decode ->
          if i = Fin1 then [dst] else acc
        | Tag ->
          if i = Out1 then [dst] else acc
      end
    | StackInst i ->
      begin
        match i with
        | Swap ->
          if Stack.length s < 2 then
            raise (Stack_error (errmsg op))
          else
            let first = Stack.pop_exn s in
            let second = Stack.pop_exn s in
            Stack.push s first;
            Stack.push s second
        | Twoswap ->
          if Stack.length s < 3 then
            raise (Stack_error (errmsg op))
          else
            let first = Stack.pop_exn s in
            let second = Stack.pop_exn s in
            let third = Stack.pop_exn s in
            Stack.push s first;
            Stack.push s second;
            Stack.push s third
      end;
      acc
  in
  try
    let checks = List.fold block ~init:[] ~f in
    Ok { g = g; phase = phase; checks = checks }
  with Stack_error msg -> Or_error.error_string msg

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
      Int.to_string c
    let graph_attributes _ = [
      `Center true;
      `Label (string_of_phase t.phase);
      `Fontsize 14;
    ]
    let default_vertex_attributes _ = [
      `Shape `Box;
      `Style `Filled;
      `Fillcolor 0xffffff;
      `Fontsize 10;
    ]
    let vertex_attributes v =
      let inst, _, _ = G.V.label v in
      match inst with
      | _ -> [
          `Label (string_of_inst inst);
        ]
    let default_edge_attributes _ = []
    let edge_attributes _ = []
    let get_subgraph _ = None
  end in
  let module Dot = Graph.Graphviz.Dot(Display) in
  let tmp = Filename.temp_file "mode" ".dot" in
  let oc = Out_channel.create tmp in
  G.Mark.clear t.g;
  Dot.output_graph oc t.g;
  Out_channel.close oc;
  ignore (Sys.command ("dot -Tpng " ^ tmp ^ " | feh -"));
  Sys.remove tmp

let cipher = new Cryptokit.Block.aes_encrypt "AAAAAAAABBBBBBBBCCCCCCCCDDDDDDDD"

let eval t =
  let ofhexstr s = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) s in
  let tohexstr s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s
                   |> String.uppercase in
  let chr = function
    | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5'
    | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9' | 10 -> 'A' | 11 -> 'B'
    | 12 -> 'C' | 13 -> 'D' | 14 -> 'E' | 15 -> 'F'
    | _ -> failwith "Fatal: invalid integer"
  in
  let ord = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
    | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'A' -> 10 | 'B' -> 11
    | 'C' -> 12 | 'D' -> 13 | 'E' -> 14 | 'F' -> 15
    | _ -> failwith "Fatal: invalid character"
  in
  let xor s s' =
    let xor c c' = chr ((ord c) lxor (ord c')) in
    String.mapi s (fun i c -> xor c s'.[i])
  in
  let fin1, fin2, out1, out2 = ref "", ref "", ref "", ref "" in
  let f v =
    let inst, _, s = G.V.label v in
    match inst with
    | Msg1 | Msg2 ->
      s := "12345678123456781234567812345678"
    | Ini1 | Ini2 ->
      s := "00000000000000000000000000000000"
    | Fin1 | Fin2 | Out1 | Out2 | Dup ->
      let _, _, s' = G.pred t.g v |> List.hd_exn |> G.V.label in
      s := !s';
      begin
        match inst with
        | Fin1 -> fin1 := !s
        | Fin2 -> fin2 := !s
        | Out1 -> out1 := !s
        | Out2 -> out2 := !s
        | _ -> ()
      end
    | Xor ->
      let ps = G.pred t.g v in
      let _, _, s1 = List.nth_exn ps 0 |> G.V.label in
      let _, _, s2 = List.nth_exn ps 1 |> G.V.label in
      s := xor !s1 !s2
    | Tbc ->
      (* XXX: note quite a tweakable blockcipher, but hopefully sufficient for
         our purposes *)
      let _, _, s' = G.pred t.g v |> List.hd_exn |> G.V.label in
      let r = String.create 16 in
      cipher#transform (ofhexstr !s') 0 r 0;
      s := tohexstr r
  in
  Topo.iter f t.g;
  match t.phase with
  | Encode | Decode ->
    String.concat ~sep:" " [!fin1; !fin2; !out1; !out2]
  | Tag ->
    !out1

let create_encode_graph t =
  let g = G.create () in
  let ctr = ref 1 in
  let find_vertex g mark =
    let f v a =
      match a with
      | Some _ -> a
      | None -> if G.Mark.get v = mark then Some v else None
    in
    Option.value_exn (G.fold_vertex f g None)
  in
  let add_vertices v checks =
    if G.Mark.get v = 0 || G.Mark.get v = -1 then begin
      let inst, _, _ = G.V.label v in
      let inst' =
        match inst with
        | Msg1 -> Out1
        | Msg2 -> Out2
        | Out1 -> Msg1
        | Out2 -> Msg2
        | Ini1 | Ini2 | Fin1 | Fin2 | Dup | Xor | Tbc as i -> i
      in
      let v' = G.V.create (inst', ref { typ = Bot; ctr = 0 }, ref "") in
      G.add_vertex g v';
      let checks =
        match inst' with
        | Out1 | Out2 -> v' :: checks
        | _ -> checks
      in
      let set v v' i = G.Mark.set v i; G.Mark.set v' i in
      set v v' (if G.Mark.get v = -1 then !ctr + 100 else !ctr);
      ctr := !ctr + 1;
      checks
    end else checks
  in
  let checks = G.fold_vertex add_vertices t.g [] in
  let rec add_edges v =
    let add_edge g src dst =
      let src = find_vertex g (G.Mark.get src) in
      let dst = find_vertex g (G.Mark.get dst) in
      let e =
        if G.Mark.get src >= 100 && G.Mark.get dst >= 100 then
          G.E.create dst () src
        else
          G.E.create src () dst
      in
      G.add_edge_e g e
    in
    let vs = G.succ t.g v in
    List.iter vs (add_edge g v)
  in
  G.iter_vertex add_edges t.g;
  G.Mark.clear g;
  { g = g; phase = Encode; checks = checks }

let derive_encode_graph t =
  Log.Global.info "Deriving encoding graph";
  assert (t.phase = Decode);
  let mark_path msg out =
    let f e =
      G.Mark.set (G.E.src e) (-1);
      G.Mark.set (G.E.dst e) (-1)
    in
    try
      let path, _ = Dijkstra.shortest_path t.g msg out in
      List.iter path f;
      Ok ()
    with _ -> Or_error.error_string
                (sprintf "No path from %s to %s" (string_of_v msg)
                   (string_of_v out))
  in
  G.Mark.clear t.g;
  let open Or_error.Monad_infix in
  Or_error.combine_errors_unit
    [ mark_path (find_vertex_by_inst t.g Msg1) (find_vertex_by_inst t.g Out1);
      mark_path (find_vertex_by_inst t.g Msg2) (find_vertex_by_inst t.g Out2);
    ]
  >>| fun () -> 
  create_encode_graph t

let check t types rand checks =
  Log.Global.info "Checking %s %b" (List.to_string string_of_typ types) rand;
  let max_ctr = ref 0 in
  begin
    let f inst typ =
      let _, map, _ = find_vertex_by_inst t.g inst |> G.V.label in
      let f = function
        | Rand -> max_ctr := 1; 1
        | _ -> 0 in
      map := { typ = typ; ctr = f typ }
    in
    match t.phase with
    | Encode | Decode ->
      List.iter2_exn [Ini1; Ini2; Msg1; Msg2] types f
    | Tag ->
      List.iter2_exn [Ini1; Ini2] types f
  end;
  let f v =
    let inst, _, _ = G.V.label v in
    begin
      match inst with
      | Msg1 | Msg2 | Ini1 | Ini2 -> ()
      | Fin1 | Fin2 | Out1 | Out2 | Dup ->
        let p = G.pred t.g v |> List.hd_exn in
        let _, pmap,_  = G.V.label p in
        let _, map, _ = G.V.label v in
        map := !pmap
      | Tbc ->
        let p = G.pred t.g v |> List.hd_exn in
        let _, pmap, _ = G.V.label p in
        let _, map, _ = G.V.label v in
        if !pmap.typ = One || !pmap.typ = Rand || rand then
          begin
            max_ctr := !max_ctr + 1;
            map := { typ = Rand; ctr = !max_ctr }
          end
        else
          map := !pmap
      | Xor ->
        let ps = G.pred t.g v in
        let p1, p2 =
          if List.length ps = 1 then
            List.nth_exn ps 0, List.nth_exn ps 0
          else
            List.nth_exn ps 0, List.nth_exn ps 1
        in
        let _, p1map, _ = G.V.label p1 in
        let _, p2map, _ = G.V.label p2 in
        let p1map, p2map =
          if !p1map.ctr < !p2map.ctr then
            p2map, p1map
          else
            p1map, p2map
        in
        let _, map, _ = G.V.label v in
        if (!p1map.typ = Zero && !p2map.typ = Zero)
        || (!p1map.typ = Zero && !p2map.typ = One)
        || (!p1map.typ = One && !p2map.typ = Zero) then
          map := { typ = xor_types !p1map.typ !p2map.typ; ctr = !p1map.ctr }
        else if !p1map.typ = Rand && !p1map.ctr > !p2map.ctr then
          map := { typ = Rand; ctr = !p1map.ctr }
        else
          map := { typ = Bot; ctr = !p1map.ctr }
    end;
    Log.Global.debug "%s" (full_string_of_v v)
  in
  Topo.iter f t.g;
  (* Check that all nodes needing to be random are indeed marked random *)
  let f v =
    let _, map, _ = G.V.label v in
    !map.typ = Rand
  in
  match List.for_all checks ~f with
  | true -> Ok ()
  | false -> Or_error.error_string "Encode graph insecure"

let is_secure_encode t =
  check t [Bot; Bot; Bot; Bot] true t.checks

let is_secure_decode t =
  let open Or_error.Monad_infix in
  check t [Zero; Zero; One; Zero] false t.checks
  >>= fun _ -> check t [Zero; Zero; Zero; One] false t.checks
  >>= fun _ -> check t [Zero; Zero; One; One] false t.checks
  >>= fun _ -> check t [Rand; Zero; Zero; Zero] false t.checks
  >>= fun _ -> check t [Rand; Zero; Zero; One] false t.checks
  >>= fun _ -> check t [Rand; Zero; One; Zero] false t.checks
  >>= fun _ -> check t [Rand; Zero; One; One] false t.checks
  >>= fun _ -> check t [Rand; One; Zero; Zero] false t.checks
  >>= fun _ -> check t [Rand; One; Zero; One] false t.checks
  >>= fun _ -> check t [Rand; One; One; Zero] false t.checks
  >>= fun _ -> check t [Rand; One; One; One] false t.checks

let is_secure_tag t =
  let open Or_error.Monad_infix in
  check t [Bot; Bot] true t.checks
  >>= fun _ -> check t [Rand; Zero] false t.checks
  >>= fun _ -> check t [Rand; One] false t.checks

let is_secure t =
  Log.Global.info "Checking security of %s graph" @@ string_of_phase t.phase;
  match t.phase with
  | Encode -> is_secure_encode t
  | Decode -> is_secure_decode t
  | Tag -> is_secure_tag t
