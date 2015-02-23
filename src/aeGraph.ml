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
  let f v = function
    | Some _ as r -> r
    | None ->
      let inst', _ = G.V.label v in
      if inst' = inst then Some v else None
  in
  match G.fold_vertex f g None with
  | None -> raise Not_found
  | Some v -> v

let find_all_vertices_by_inst g inst =
  let f v acc =
    let inst', _ = G.V.label v in
    if inst' = inst then v :: acc else acc
  in
  let l = G.fold_vertex f g [] in
  if l = [] then raise Not_found else l

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
      pop (AeInst.n_in op) ~dst ~op >>= fun () ->
      push (AeInst.n_out op) ~dst   >>= fun () ->
      acc                           >>| fun acc ->
      if phase = Encode && (i = Out1 || i = Out2) then dst :: acc
      else if phase = Decode && i = Fin1 then [dst]
      else if phase = Tag && i = Out1 then [dst]
      else acc
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
       first writing the graph to a (temporary) file, and then creating a new
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

exception Unencryptable of string

let derive_encode_graph t =
  assert (t.phase = Decode);
  Lgr.info "Deriving Encode graph";
  let g = G.create () in
  let map_inst = function
    | Msg1 -> Out1
    | Msg2 -> Out2
    | Out1 -> Msg1
    | Out2 -> Msg2
    | _ as i -> i
  in
  let find_vertex g mark =
    let f v = function
      | Some _ as a -> a
      | None -> if G.Mark.get v = mark then Some v else None
    in
    match G.fold_vertex f g None with
    | Some v -> v
    | None -> raise Not_found
  in
  let mark v =
    (* XXX: This'll break for graphs with >= 1000 nodes. *)
    assert (G.Mark.get v <= 1000);
    G.Mark.set v (G.Mark.get v + 1000) in
  let is_marked v = G.Mark.get v > 1000 in
  let f v (ctr, vs, checks) =
    let inst, _ = G.V.label v in
    let v' = G.V.create (map_inst inst, ref { typ = Bot; ctr = 0 }) in
    G.add_vertex g v';
    (* Give each vertex a unique mark, and make the two marks equal in both the
       old graph and the new graph, so that we can locate the same nodes between
       the two graphs. *)
    G.Mark.set v ctr;
    G.Mark.set v' ctr;
    let checks =
      match map_inst inst with
      | Out1 | Out2 -> v' :: checks
      | _ -> checks
    in
    match inst with
    | Out1 | Out2 | Ini1 | Ini2 ->
      mark v; mark v'; (ctr + 1, vs, checks)
    | _ -> (ctr + 1, v :: vs, checks)
  in
  let _, vs, checks = G.fold_vertex f t.g (1, [], []) in
  let f acc v =
    let neighbors v = List.append (G.succ t.g v) (G.pred t.g v) in
    let add_edge src dst =
      let e = G.E.create src () dst in G.add_edge_e g e in
    let add_edge_and_mark v src dst acc =
      add_edge src dst; mark v; mark dst; acc in
    let of_bool b = if b then 1 else 0 in
    let vs = neighbors v |> List.to_array in
    let v_new = find_vertex g (G.Mark.get v) in
    let inst, _ = G.V.label v in
    match inst with
    | Msg1 | Msg2 | Fin1 | Fin2 ->
      let v' = vs.(0) in
      let v'_new = find_vertex g (G.Mark.get v') in
      if is_marked v'_new then add_edge_and_mark v v'_new v_new acc
      else v :: acc
    | Tbc ->
      let v1, v2 = vs.(0), vs.(1) in
      let v1_new = find_vertex g (G.Mark.get v1) in
      let v2_new = find_vertex g (G.Mark.get v2) in
      if is_marked v1_new && is_marked v2_new then
        raise (Unencryptable "TBC: Both vertices marked")
      else if is_marked v1_new then add_edge_and_mark v v1_new v_new acc
      else if is_marked v2_new then add_edge_and_mark v v2_new v_new acc
      else v :: acc
    | Dup ->
      if Array.length vs = 2 then
        raise (Unencryptable "DUP: Only one outgoing edge")
      else
        let v1, v2, v3 = vs.(0), vs.(1), vs.(2) in
        let v1_new = find_vertex g (G.Mark.get v1) in
        let v2_new = find_vertex g (G.Mark.get v2) in
        let v3_new = find_vertex g (G.Mark.get v3) in
        let cnt = (is_marked v1_new |> of_bool)
                  + (is_marked v2_new |> of_bool)
                  + (is_marked v3_new |> of_bool) in
        if cnt > 1 then raise (Unencryptable "DUP: More than one vertex marked")
        else if is_marked v1_new then add_edge_and_mark v v1_new v_new acc
        else if is_marked v2_new then add_edge_and_mark v v2_new v_new acc
        else if is_marked v3_new then add_edge_and_mark v v3_new v_new acc
        else v :: acc
    | Xor ->
      if Array.length vs = 2 then
        raise (Unencryptable "XOR: Only one incoming edge")
      else
        let v1, v2, v3 = vs.(0), vs.(1), vs.(2) in
        let v1_new = find_vertex g (G.Mark.get v1) in
        let v2_new = find_vertex g (G.Mark.get v2) in
        let v3_new = find_vertex g (G.Mark.get v3) in
        let cnt = (is_marked v1_new |> of_bool)
                  + (is_marked v2_new |> of_bool)
                  + (is_marked v3_new |> of_bool) in
        if cnt = 3 then raise (Unencryptable "XOR: Too many vertices marked")
        else if cnt = 2 then begin
          if is_marked v1_new then add_edge v1_new v_new;
          if is_marked v2_new then add_edge v2_new v_new;
          if is_marked v3_new then add_edge v3_new v_new;
          mark v; mark v_new; acc
        end else v :: acc
    | Ini1 | Ini2 | Out1 | Out2 -> assert false
  in
  let rec loop l =
    if not (List.is_empty l) then begin
      Lgr.debug "Looping over %s" (List.to_string l ~f:string_of_v);
      let l' = List.fold l ~init:[] ~f |> List.rev in
      if l = l' then
        raise (Unencryptable "Reached fixed point but list not empty");
      loop l'
    end
  in
  try
    loop vs;
    Ok { g = g; phase = Encode; checks = checks }
  with Unencryptable err ->
    Or_error.errorf "Cannot derive encode graph: %s" err

(* Runs the Map function mapping nodes to (type, ctr) tuples *)
let map t types rand ~simple =
  Lgr.info "Checking %s %b" (List.to_string string_of_typ (Array.to_list types)) rand;
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
        if simple then [| Ini1; Msg1; Msg2 |] else [| Ini1; Ini2; Msg1; Msg2 |]
      | Tag ->
        if simple then [| Ini1 |] else [| Ini1; Ini2 |]
    in
    Array.iter2_exn l types f
  end;
  let f v =
    let inst, _ = G.V.label v in
    match inst with
    | Msg1 | Msg2 | Ini1 | Ini2 -> ()
    | Fin1 | Fin2 | Out1 | Out2 | Dup ->
      let _, pmap = G.pred t.g v |> List.hd_exn |> G.V.label in
      let _, map = G.V.label v in
      map := !pmap
    | Tbc ->
      let _, pmap = G.pred t.g v |> List.hd_exn |> G.V.label in
      let _, map = G.V.label v in
      if rand || !pmap.typ = One || !pmap.typ = Rand then begin
        max_ctr := !max_ctr + 1;
        map := { typ = Rand; ctr = !max_ctr }
      end else map := !pmap
    | Xor ->
      let ps = G.pred t.g v in
      let p1, p2 =
        if List.length ps = 1 then List.nth_exn ps 0, List.nth_exn ps 0
        else List.nth_exn ps 0, List.nth_exn ps 1
      in
      let _, p1map = G.V.label p1 in
      let _, p2map = G.V.label p2 in
      let p1map, p2map =
        if !p1map.ctr < !p2map.ctr then !p2map, !p1map else !p1map, !p2map in
      let _, map = G.V.label v in
      if (p1map.typ = Zero && p2map.typ = Zero)
      || (p1map.typ = Zero && p2map.typ = One)
      || (p1map.typ = One && p2map.typ = Zero) then
        let xor_types = function
        | Zero, Zero | One, One -> Zero
        | Zero, One | One, Zero -> One
        | _ , _ -> assert false
        in
        map := { typ = xor_types (p1map.typ, p2map.typ); ctr = p1map.ctr }
      else if p1map.typ = Rand && p1map.ctr > p2map.ctr then
        map := { typ = Rand; ctr = p1map.ctr }
      else
        map := { typ = Bot; ctr = p1map.ctr }
  in
  let f' v =
    f v;
    let ps = G.pred t.g v in
    let ps = List.to_string ps ~f:(fun p -> string_of_v p) in
    Lgr.debug "%s | %s" (full_string_of_v v) ps
  in
  Topo.iter f' t.g

(* Runs Map and checks that all nodes needing to be random are indeed marked
   random *)
let check t types rand checks ~simple =
  map t types rand ~simple;
  let f v = let _, map = G.V.label v in !map.typ = Rand in
  match List.for_all checks ~f with
  | true -> Ok ()
  | false -> Or_error.errorf "Graph insecure on inputs %s"
               (List.to_string (Array.to_list types) ~f:string_of_typ)

let is_secure_encode t types ~simple =
  let open Or_error.Monad_infix in
  check t types true t.checks ~simple >>= fun () ->
  let ctr v = let _, map = G.V.label v in !map.ctr in
  assert (List.length t.checks = 2);
  let a, b = List.nth_exn t.checks 0, List.nth_exn t.checks 1 in
  if ctr a <> ctr b then Ok ()
  else Or_error.error_string "Graph insecure: counters are equal"

let is_secure_decode t types ~simple =
  let f acc types = match acc with
    | Ok () -> check t types false t.checks ~simple
    | Error _ as e -> e
  in
  List.fold ~init:(Ok ()) types ~f

let is_secure_tag t randtypes types ~simple =
  let open Or_error.Monad_infix in
  check t randtypes true t.checks ~simple >>= fun () ->
  let f acc types = match acc with
    | Ok () -> check t types false t.checks ~simple
    | Error _ as e -> e
  in
  List.fold ~init:(Ok ()) types ~f

let is_secure t ~simple =
  Lgr.info "Checking security of %s graph" @@ string_of_phase t.phase;
  match t.phase with
  | Encode ->
    let types = if simple then [| Bot; Bot; Bot |] else [| Bot; Bot; Bot; Bot |] in
    is_secure_encode t types ~simple
  | Decode ->
    let types =
      if simple then [
        [| Zero; One; Zero |];
        [| Zero; Zero; One |];
        [| Zero; One; One |];
        [| Rand; Zero; Zero |];
        [| Rand; Zero; One |];
        [| Rand; One; Zero |];
        [| Rand; One; One |]
      ] else [
        [| Zero; Zero; One; Zero |];
        [| Zero; Zero; Zero; One |];
        [| Zero; Zero; One; One |];
        [| Rand; Zero; Zero; Zero |];
        [| Rand; Zero; Zero; One |];
        [| Rand; Zero; One; Zero |];
        [| Rand; Zero; One; One |];
        [| Rand; One; Zero; Zero |];
        [| Rand; One; Zero; One |];
        [| Rand; One; One; Zero |];
        [| Rand; One; One; One |]
      ]
    in
    is_secure_decode t types ~simple
  | Tag ->
    let randtypes = if simple then [| Bot |] else [| Bot; Bot |] in
    let types = if simple then [
        [| Rand |]
      ] else [
        [| Rand; Zero |];
        [| Rand; One |];
      ]
    in
    is_secure_tag t randtypes types ~simple

let oae tenc tdec ttag types enc_checks dec_checks ~simple =
  let open Or_error.Monad_infix in
  check tdec types false dec_checks ~simple >>= fun () ->
  check tenc types false enc_checks ~simple >>= fun () ->
  let check_ctrs checks =
    let ctr v = let _, map = G.V.label v in !map.ctr in
    let cmp = 
    if simple then
      let a, b, c = checks.(0), checks.(1), checks.(2) in
      ctr a = ctr b || ctr b = ctr c || ctr c = ctr a
    else
      let a, b, c, d = checks.(0), checks.(1), checks.(2), checks.(3) in
      ctr a = ctr b || ctr b = ctr c || ctr c = ctr d || ctr d = ctr a
    in
    if cmp then Or_error.error_string "Graph not misuse resistant: counters are equal"
    else Ok ()
  in
  check_ctrs (Array.of_list enc_checks)

let simpleton t types check ~simple =
  map t types false ~simple;
  let tbcs = find_all_vertices_by_inst t.g Tbc in
  let ctr = let _, map = G.V.label check in !map.ctr in
  let f count v =
    let _, map = G.V.label v in
    if !map.typ = Rand && !map.ctr = ctr then count + 1
    else count
  in
  List.fold ~init:0  tbcs ~f

let oae tenc tdec ttag enc_checks dec_check types tag_types ~simple =
  let open Or_error.Monad_infix in
  let f acc types =
    match acc with
    | Ok () -> oae tenc tdec ttag types enc_checks [dec_check] ~simple
    | Error _ as e -> e
  in
  (* Lines 03--11 *)
  List.fold ~init:(Ok ()) types ~f >>= fun () ->
  (* TODO: check all counters are different *)
  let f acc types =
    match acc with
    | Ok () -> check ttag types false ttag.checks ~simple
    | Error _ as e -> e
  in
  (* Lines 12--16 *)
  List.fold ~init:(Ok ()) tag_types ~f >>= fun () ->
  let f acc check =
    match acc with
    | Ok () ->
      let f count types = count + simpleton tenc types check ~simple in
      let count = List.fold ~init:0 types ~f in
      if count <> 1 then Ok ()
      else Or_error.error_string "Graph not misuse resistant: simpleton failed"
    | Error _ as e -> e
  in
  (* Lines 17--18 *)
  List.fold ~init:(Ok ()) enc_checks ~f >>= fun () ->
  (* Line 19 *)
  let f count types = count + simpleton tdec types dec_check false in
  let count = List.fold ~init:0 types ~f in
  if count = 1 then Ok ()
  else Or_error.error_string "Graph not misuse resistant: simpleton failed"

let is_misuse_resistant tenc tdec ttag ~simple =
  let open Or_error.Monad_infix in
  Lgr.info "Checking misuse resistance of scheme";
  let out1 = find_vertex_by_inst tenc.g Out1 in
  let out2 = find_vertex_by_inst tenc.g Out2 in
  let fin1 = find_vertex_by_inst tenc.g Fin1 in
  let dec_check = find_vertex_by_inst tdec.g Out1 in
  let enc_checks = [out1; out2; fin1] in
  let types = if simple then [
      [| Zero; Zero; One |];
      [| Zero; One; Zero |];
      [| Zero; One; One |];
      [| Rand; Zero; Zero |];
      [| Rand; Zero; One |];
      [| Rand; One; Zero |];
      [| Rand; One; One |];
    ] else [
      [| Zero; Zero; Zero; One |];
      [| Zero; Zero; One; Zero |];
      [| Zero; Zero; One; One |];
      [| Rand; Zero; Zero; Zero |];
      [| Rand; Zero; Zero; One |];
      [| Rand; Zero; One; Zero |];
      [| Rand; Zero; One; One |];
      [| Rand; One; Zero; Zero |];
      [| Rand; One; Zero; One |];
      [| Rand; One; One; Zero |];
      [| Rand; One; One; One |]
    ]
  in
  let tag_types = if simple then [
      [| Rand |];
      [| Bot |]
    ] else [
      [| Rand; Zero |];
      [| Rand; One |];
      [| Bot; Bot |]
    ] in
  oae tenc tdec ttag enc_checks dec_check types tag_types ~simple
