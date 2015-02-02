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
  (* Each vertex contains two elements: the instruction the vertex represents
     and a map reference for checking security of the graph.  *)
  (* XXX: Can we do this without storing the map ref within the vertex type? *)
  type t = inst * map ref
end
module G = Graph.Imperative.Digraph.Abstract(V)
module Topo = Graph.Topological.Make(G)
module Oper = Graph.Oper.I(G)
module Dijkstra = Graph.Path.Dijkstra(G)(struct
    type edge = G.E.t
    type t = int
    let weight _ = Int.one
    let compare = Int.compare
    let add = Int.(+)
    let zero = Int.zero
  end)
module Dfs = Graph.Traverse.Dfs(G)

let string_of_v v =
  let inst, _ = G.V.label v in
  string_of_inst inst

let string_of_e e =
  let src, _ = G.E.src e |> G.V.label in
  let dst, _ = G.E.dst e |> G.V.label in
  [string_of_inst src; string_of_inst dst]
  |> String.concat ~sep:" -> "

let full_string_of_v v =
  let _, map = G.V.label v in
  [string_of_typ !map.typ; Int.to_string !map.ctr]
  |> List.append [string_of_v v] |> String.concat ~sep:" "

let find_vertex_by_inst g inst =
  let f v r =
    match r with
    | Some _ -> r
    | None ->
      let inst', _ = G.V.label v in
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
      let dst = G.V.create (i, ref { typ = Bot; ctr = 0 }) in
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

let display ?(save=None) t t' t'' =
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
      let inst, _ = G.V.label v in
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
    let str = match save with
      | None -> "| feh -"
      | Some file -> "> " ^ file
    in
    "gvpack -array_i -u " ^ (String.concat ~sep:" " tmps) ^
    " 2>/dev/null | dot -Tpng " ^ str
  in
  ignore (Sys.command command);
  List.iter tmps ~f:Sys.remove

let cipher = new Cryptokit.Block.aes_encrypt "AAAAAAAABBBBBBBBCCCCCCCCDDDDDDDD"

let eval t ~simple =
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
  let rec f v =
    let inst, _ = G.V.label v in
    match inst with
    | Msg1 -> "12345678123456781234567812345678"
    | Msg2 -> "87654321876543218765432187654321"
    | Ini1 | Ini2 -> "00000000000000000000000000000000"
    | Fin1 | Fin2 | Out1 | Out2 | Dup ->
      let v = G.pred t.g v |> List.hd_exn in
      f v
    | Xor ->
      let ps = G.pred t.g v in
      begin
        match List.length ps with
        (* The result of XORing a value with itself *)
        | 1 -> "00000000000000000000000000000000"
        | 2 ->
          let l = List.map ps ~f in
          xor (List.nth_exn l 0) (List.nth_exn l 1)
        | _ -> assert false
      end
    | Tbc ->
      let v = G.pred t.g v |> List.hd_exn in
      let r = String.create 16 in
      cipher#transform (ofhexstr (f v)) 0 r 0;
      tohexstr r
  in
  match t.phase with
  | Encode | Decode ->
    let l = [find_vertex_by_inst t.g Out1;
             find_vertex_by_inst t.g Out2;
             find_vertex_by_inst t.g Fin1] in
    let l = if simple then l else l @ [find_vertex_by_inst t.g Fin2] in
    List.map l ~f |> String.concat ~sep:" "
  | Tag ->
    find_vertex_by_inst t.g Out1 |> f

let mark_vertices v v' i = G.Mark.set v i; G.Mark.set v' i

let is_well_formed t =
  let open Or_error.Monad_infix in
  let f v r =
    match r with
    | Error _ -> r
    | Ok () ->
      let inst, _ = G.V.label v in
      let ps = G.pred t.g v in
      let ss = G.succ t.g v in
      let bool =
        match inst with
        | Ini1 | Ini2 | Msg1 | Msg2 ->
          List.length ps = 0 && List.length ss = 1
        | Fin1 | Fin2 | Out1 | Out2 ->
          List.length ps = 1 && List.length ss = 0
        | Tbc ->
          List.length ps = 1 && List.length ss = 1
        | Dup ->
          List.length ps = 1 && List.length ss = 2
        | Xor ->
          List.length ps = 2 && List.length ss = 1
      in
      if bool then Ok ()
      else Or_error.errorf "Invalid vertex %s" (string_of_inst inst)
  in
  G.fold_vertex f t.g (Ok ()) >>= fun () ->
  if Dfs.has_cycle t.g then
    Or_error.error_string "Graph has a cycle"
  else
    Ok ()

let derive_encode_graph t =
  let open Or_error.Monad_infix in
  Lgr.info "Deriving Encode graph";
  assert (t.phase = Decode);
  let mark1 = -1 in
  let mark2 = -2 in
  let create_encode_graph t =
    let path1 = 1000 in
    let path2 = 2000 in
    let is_path1 src dst =
      G.Mark.get src >= path1 && G.Mark.get src < path2
      && G.Mark.get dst >= path1 && G.Mark.get dst < path2
    in
    let is_path2 src dst = G.Mark.get src >= path2 && G.Mark.get dst >= path2 in
    let g = G.create () in
    let find_vertex g mark =
      let f v a =
        match a with
        | Some _ -> a
        | None -> if G.Mark.get v = mark then Some v else None
      in
      Option.value_exn (G.fold_vertex f g None)
    in
    let add_vertices v (ctr, acc) =
      assert (G.Mark.get v = 0 || G.Mark.get v = mark1 || G.Mark.get v = mark2);
      let inst, _ = G.V.label v in
      let inst' =
        match inst with
        | Msg1 -> Out1
        | Msg2 -> Out2
        | Out1 -> Msg1
        | Out2 -> Msg2
        | _ as i -> i
      in
      let v' = G.V.create (inst', ref { typ = Bot; ctr = 0 }) in
      G.add_vertex g v';
      let ctr' =
        if G.Mark.get v = mark1 then ctr + path1
        else if G.Mark.get v = mark2 then ctr + path2
        else ctr
      in
      mark_vertices v v' ctr';
      let acc = match inst' with
        | Out1 | Out2 -> v' :: acc
        | _ -> acc
      in
      ctr + 1, acc
    in
    let _, checks = G.fold_vertex add_vertices t.g (1, []) in
    let add_edges v =
      let add_edge g src dst =
        let src = find_vertex g (G.Mark.get src) in
        let dst = find_vertex g (G.Mark.get dst) in
        let e =
          if is_path1 src dst || is_path2 src dst then
            G.E.create dst () src
          else
            G.E.create src () dst
        in
        G.add_edge_e g e
      in
      List.iter (G.succ t.g v) ~f:(add_edge g v)
    in
    G.iter_vertex add_edges t.g;
    G.Mark.clear g;
    Ok { g = g; phase = Encode; checks = checks }
  in
  let mark_path msg out i =
    assert (i < 0);
    (* Determine whether there is a path from msg to out *)
    begin
      try
        let path, _ = Dijkstra.shortest_path t.g msg out in
        Ok path
      with Not_found ->
        Or_error.errorf "No path from %s to %s" (string_of_v msg)
          (string_of_v out)
    end >>= fun path ->
    (* If so, check that only one such path exists *)
    (* let exists_path src dst = *)
    (*   try *)
    (*     ignore (Dijkstra.shortest_path t.g src dst); *)
    (*     true *)
    (*   with Not_found -> false *)
    (* in *)
    (* let f r e = *)
    (*   match r with *)
    (*   | Error _ -> r *)
    (*   | Ok () -> *)
    (*     let v = G.E.dst e in *)
    (*     let inst, _ = G.V.label v in *)
    (*     if inst = Dup then *)
    (*       let vs = G.succ t.g v in *)
    (*       let cnt = List.count vs ~f:(fun v -> exists_path v out) in *)
    (*       match cnt with *)
    (*       | 1 -> Ok () *)
    (*       | 2 -> Ok () (\* Or_error.errorf "Two paths between %s and %s" *\) *)
    (*              (\*   (string_of_v msg) (string_of_v out) *\) *)
    (*       | _ -> assert false *)
    (*     else Ok () *)
    (* in *)
    (* List.fold path ~init:(Ok ()) ~f >>= fun () -> *)
    (* If so, tag the vertices on the path *)
    let f err e =
      match err with
      | Error _ -> err
      | Ok _ ->
        let src, dst = G.E.src e, G.E.dst e in
        Lgr.debug "%s -> %s" (string_of_v src) (string_of_v dst);
        if G.Mark.get dst < 0 then
          Or_error.errorf "Shared edge found between %s and %s"
            (string_of_v msg) (string_of_v out)
        else begin
          mark_vertices (G.E.src e) (G.E.dst e) i;
          Ok ()
        end
    in
    List.fold path ~init:(Ok ()) ~f
  in
  G.Mark.clear t.g;
  Or_error.combine_errors_unit
    [ mark_path (find_vertex_by_inst t.g Msg1) (find_vertex_by_inst t.g Out1) (-1);
      mark_path (find_vertex_by_inst t.g Msg2) (find_vertex_by_inst t.g Out2) (-2);
    ] >>= fun () ->
  create_encode_graph t(*  >>= fun t -> *)
  (* is_well_formed t >>| fun () -> t *)

let check t types rand checks ~simple =
  Lgr.info "Checking %s %b" (List.to_string string_of_typ types) rand;
  let max_ctr = ref 0 in
  begin
    let f inst typ =
      let _, map = find_vertex_by_inst t.g inst |> G.V.label in
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
    let inst, _ = G.V.label v in
    begin
      match inst with
      | Msg1 | Msg2 | Ini1 | Ini2 -> ()
      | Fin1 | Fin2 | Out1 | Out2 | Dup ->
        let _, pmap = G.pred t.g v |> List.hd_exn |> G.V.label in
        let _, map = G.V.label v in
        map := !pmap
      | Tbc ->
        let _, pmap = G.pred t.g v |> List.hd_exn |> G.V.label in
        let _, map = G.V.label v in
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
        let _, p1map = G.V.label p1 in
        let _, p2map = G.V.label p2 in
        let p1map, p2map =
          if !p1map.ctr < !p2map.ctr then
            p2map, p1map
          else
            p1map, p2map
        in
        let _, map = G.V.label v in
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
    let _, map = G.V.label v in
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
