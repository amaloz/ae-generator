open AeInclude

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
module Oper = Graph.Oper.I(G)
module Dijkstra = Graph.Path.Dijkstra(G)(struct
    type label = G.E.label
    type t = int
    let weight _ = Int.one
    let compare = Int.compare
    let add = Int.(+)
    let zero = Int.zero
  end)

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

let create block phase =
  let open Or_error.Monad_infix in
  let s = Stack.create () in
  let g = G.create () in
  let errmsg op = sprintf "%s: Not enough items on stack" (string_of_op op) in
  let rec pop ~dst ~op = function
    | 0 -> Ok ()
    | n -> match Stack.pop s with
      | Some src ->
        G.E.create src () dst |> G.add_edge_e g; pop (n - 1) ~dst ~op
      | None -> Or_error.error_string (errmsg op)
  in
  let rec push ~dst = function
    | 0 -> Ok ()
    | n -> Stack.push s dst; push ~dst (n - 1)
  in
  let f acc op =
    match op with
    | Inst i ->
      let dst = G.V.create (i, ref { typ = Bot; ctr = 0 }, ref "") in
      G.add_vertex g dst;
      pop (AeInst.n_in op) ~dst ~op >>= fun _ ->
      push (AeInst.n_out op) ~dst   >>= fun _ ->
      acc                           >>| fun acc ->
      if phase = Encode && (i = Out1 || i = Out2) then
        dst :: acc
      else if phase = Decode && i = Fin1 then
        [dst]
      else if phase = Tag && i = Out1 then
        [dst]
      else
        acc
    | StackInst i ->
      let open Option.Monad_infix in
      let result = match i with
        | Swap ->
          Stack.pop s >>= fun first ->
          Stack.pop s >>| fun second ->
          Stack.push s first;
          Stack.push s second
        | Twoswap ->
          Stack.pop s >>= fun first ->
          Stack.pop s >>= fun second ->
          Stack.pop s >>| fun third ->
          Stack.push s first;
          Stack.push s second;
          Stack.push s third
      in
      if Option.is_some result then acc
      else Or_error.error_string (errmsg op)
  in
  List.fold block ~init:(Ok []) ~f >>| fun checks ->
  { g = g; phase = phase; checks = checks }

module type Phase = sig val phase : phase end

let display ?(save=false) t t' t'' =
  let module Display (M : Phase) = struct
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
      `Label (string_of_phase M.phase);
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
  let create t file =
    G.Mark.clear t.g;
    let module Dot = Graph.Graphviz.Dot(Display(struct let phase = t.phase end)) in
    (* XXX: This is ugly.  We want to output the dot file for the graph, but
       change the name of the graph from "G" to "clusterG" so that when we use
       gvpack to combine the graphs, each graph is in a box.  We do this by
       first writing the graph to a (temporary) file, and the creating a new
       file which is exactly the same as the old file except that the first line
       is changed.  This is ugly, and there is probably a cleaner way to do
       it. *)
    let file' = file ^ "-tmp" in
    Out_channel.with_file file' ~f:(fun oc -> Dot.output_graph oc t.g);
    Out_channel.with_file file ~f:(fun oc ->
        In_channel.with_file file' ~f:(fun ic ->
            let _ = Option.value_exn (In_channel.input_line ic) in
            (* Change first line from "digraph G {" to "digraph clusterG {" *)
            fprintf oc "digraph clusterG {\n";
            In_channel.iter_lines ic ~f:(fun s -> fprintf oc "%s\n%!" s)
          );
        Sys.remove file'
      )
  in
  let tmpfile phase = Filename.temp_file (string_of_phase phase) ".dot" in
  let phases = [t.phase; t'.phase; t''.phase] in
  let tmps = List.map phases ~f:tmpfile in
  List.iter2_exn [t; t'; t''] tmps ~f:create;
  let command =
    if save then
      let file = "mode.png" in
      "gvpack -array_i -u " ^ (String.concat ~sep:" " tmps) ^
      " 2>/dev/null | dot -Tpng > " ^ file
    else
      "gvpack -array_i -u " ^ (String.concat ~sep:" " tmps) ^
      " 2>/dev/null | dot -Tpng | feh -"
  in
  ignore (Sys.command command);
  List.iter tmps ~f:Sys.remove

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
  Lgr.info "Deriving encoding graph";
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
    with _ ->
      Or_error.error_string
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

let check t types rand checks ~simple =
  Lgr.info "Checking %s %b" (List.to_string string_of_typ types) rand;
  let max_ctr = ref 0 in
  begin
    let f inst typ =
      let _, map, _ = find_vertex_by_inst t.g inst |> G.V.label in
      let f = function
        | Rand -> max_ctr := 1; 1
        | _ -> 0 in
      map := { typ = typ; ctr = f typ }
    in
    let l =
      match t.phase with
      | Encode | Decode ->
        if simple then [Ini1; Msg1; Msg2] else [Ini1; Ini2; Msg1; Msg2]
      | Tag ->
        if simple then [Ini1] else [Ini1; Ini2]
    in
    List.iter2_exn l types f
  end;
  let f v =
    let inst, _, _ = G.V.label v in
    begin
      match inst with
      | Msg1 | Msg2 | Ini1 | Ini2 -> ()
      | Fin1 | Fin2 | Out1 | Out2 | Dup ->
        let _, pmap, _ = G.pred t.g v |> List.hd_exn |> G.V.label in
        let _, map, _ = G.V.label v in
        map := !pmap
      | Tbc ->
        let _, pmap, _ = G.pred t.g v |> List.hd_exn |> G.V.label in
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
        let xor_types = function
          | Zero, Zero | One, One -> Zero
          | Zero, One | One, Zero -> One
          | _ , _ -> assert false
        in
        if (!p1map.typ = Zero && !p2map.typ = Zero)
        || (!p1map.typ = Zero && !p2map.typ = One)
        || (!p1map.typ = One && !p2map.typ = Zero) then
          map := { typ = xor_types (!p1map.typ, !p2map.typ); ctr = !p1map.ctr }
        else if !p1map.typ = Rand && !p1map.ctr > !p2map.ctr then
          map := { typ = Rand; ctr = !p1map.ctr }
        else
          map := { typ = Bot; ctr = !p1map.ctr }
    end;
    Lgr.debug "%s" (full_string_of_v v)
  in
  Topo.iter f t.g;
  (* Check that all nodes needing to be random are indeed marked random *)
  let f v =
    let _, map, _ = G.V.label v in
    !map.typ = Rand
  in
  match List.for_all checks ~f with
  | true -> Ok ()
  | false -> Or_error.error_string
               (sprintf "Graph insecure on inputs %s"
                  (List.to_string types ~f:string_of_typ))

let is_secure_encode t =
  check t ~simple:false [Bot; Bot; Bot; Bot] true t.checks
let is_secure_encode_simple t =
  check t ~simple:true [Bot; Bot; Bot] true t.checks

let is_secure_decode t =
  let open Or_error.Monad_infix in
  check t ~simple:false [Zero; Zero; One; Zero] false t.checks  >>= fun _ ->
  check t ~simple:false [Zero; Zero; Zero; One] false t.checks  >>= fun _ ->
  check t ~simple:false [Zero; Zero; One; One] false t.checks   >>= fun _ ->
  check t ~simple:false [Rand; Zero; Zero; Zero] false t.checks >>= fun _ ->
  check t ~simple:false [Rand; Zero; Zero; One] false t.checks  >>= fun _ ->
  check t ~simple:false [Rand; Zero; One; Zero] false t.checks  >>= fun _ ->
  check t ~simple:false [Rand; Zero; One; One] false t.checks   >>= fun _ ->
  check t ~simple:false [Rand; One; Zero; Zero] false t.checks  >>= fun _ ->
  check t ~simple:false [Rand; One; Zero; One] false t.checks   >>= fun _ ->
  check t ~simple:false [Rand; One; One; Zero] false t.checks   >>= fun _ ->
  check t ~simple:false [Rand; One; One; One] false t.checks
let is_secure_decode_simple t =
  let open Or_error.Monad_infix in
  check t ~simple:true [Zero; One; Zero] false t.checks  >>= fun _ ->
  check t ~simple:true [Zero; Zero; One] false t.checks  >>= fun _ ->
  check t ~simple:true [Zero; One; One] false t.checks   >>= fun _ ->
  check t ~simple:true [Rand; Zero; Zero] false t.checks >>= fun _ ->
  check t ~simple:true [Rand; Zero; One] false t.checks  >>= fun _ ->
  check t ~simple:true [Rand; One; Zero] false t.checks  >>= fun _ ->
  check t ~simple:true [Rand; One; One] false t.checks

let is_secure_tag t =
  let open Or_error.Monad_infix in
  check t ~simple:false [Bot; Bot] true t.checks    >>= fun _ ->
  check t ~simple:false [Rand; Zero] false t.checks >>= fun _ ->
  check t ~simple:false [Rand; One] false t.checks
let is_secure_tag_simple t =
  let open Or_error.Monad_infix in
  check t ~simple:true [Bot] true t.checks   >>= fun _ ->
  check t ~simple:true [Rand] false t.checks

let is_secure t ~simple =
  Lgr.info "Checking security of %s graph" @@ string_of_phase t.phase;
  if simple then
    match t.phase with
    | Encode -> is_secure_encode_simple t
    | Decode -> is_secure_decode_simple t
    | Tag -> is_secure_tag_simple t
  else
    match t.phase with
    | Encode -> is_secure_encode t
    | Decode -> is_secure_decode t
    | Tag -> is_secure_tag t
