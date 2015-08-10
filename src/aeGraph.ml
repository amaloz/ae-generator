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

(* 'array' is used to avoid the issue detailed in Fig. 3.8 in the paper. *)
type map = { typ : typ; ctr : int; (* array : int array *) }

module V = struct
  (* Each vertex contains two elements: the instruction the vertex represents
     and a map reference for checking security of the graph.  *)
  (* XXX: Can we do this without storing the map ref within the vertex type? *)
  type t = inst * map ref
end
module G = Graph.Imperative.Digraph.Abstract(V)
module Topo = Graph.Topological.Make(G)
module Check = Graph.Path.Check(G)

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
  [string_of_typ !map.typ; Int.to_string !map.ctr;
   (* Array.to_list !map.array |> List.to_string ~f:Int.to_string *)]
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

let xor_array a b =
  Array.map2_exn a b (fun i j -> (i + j) % 2)

let and_bit a b =
  match a, b with
  | false, _ | _, false -> false
  | true, true -> true

let xor_bit a b =
  match a, b with
  | false, false | true, true -> false
  | false, true | true, false -> true

let xor_bit_array a b =
  Array.map2_exn a b xor_bit

type t = { g : G.t; phase : phase; checks : G.V.t list }

let count t inst = find_all_vertices_by_inst t.g inst |> List.length

let create block phase =
  let open Or_error.Monad_infix in
  let s = Stack.create () in
  let g = G.create () in
  let errmsg op = sprintf "%s: Not enough items on stack" (string_of_op op) in
  let fchecks = function
    | Encode -> (fun i x acc -> if i = Out1 || i = Out2 then x :: acc else acc)
    | Decode -> (fun i x acc -> if i = Fin1 then [x] else acc)
    | Tag -> (fun i x acc -> if i = Out1 then [x] else acc)
  in
  let fchecks = fchecks phase in
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
      let dst = G.V.create (i, ref { typ = Bot; ctr = 0; (* array = [||] *) }) in
      G.add_vertex g dst;
      pop (AeInst.n_in op) ~dst ~op >>= fun () ->
      push (AeInst.n_out op) ~dst   >>= fun () ->
      acc                           >>| fun acc ->
      fchecks i dst acc
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
  { g; phase; checks }

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

let msg1 = "12345678123456781234567812345678"
let msg2 = "87654321876543218765432187654321"

let eval t ~simple ~msg1 ~msg2 =
  let ofhexstr s = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) s in
  let tohexstr s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s
                   |> String.uppercase in
  let chr = function
    | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5'
    | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9' | 10 -> 'A' | 11 -> 'B'
    | 12 -> 'C' | 13 -> 'D' | 14 -> 'E' | 15 -> 'F'
    | _ -> failwith "Fatal: invalid integer" in
  let ord = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
    | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'A' -> 10 | 'B' -> 11
    | 'C' -> 12 | 'D' -> 13 | 'E' -> 14 | 'F' -> 15
    | _ -> failwith "Fatal: invalid character" in
  let xor s s' =
    let xor c c' = chr ((ord c) lxor (ord c')) in
    String.mapi s (fun i c -> xor c s'.[i]) in
  let rec f v =
    let inst, _ = G.V.label v in
    match inst with
    | In1 -> msg1
    | In2 -> msg2
    | Ini1 -> "11111111222222223333333344444444"
    | Ini2 -> "44444444333333332222222211111111"
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
    List.map l ~f
  | Tag ->
    find_vertex_by_inst t.g Out1 |> f |> (fun x -> [x])

exception Unreversable of string

(* Find a vertex in 'g' with mark 'mark' *)
let find_vertex g mark =
  let f v = function
    | Some _ as a -> a
    | None -> if G.Mark.get v = mark then Some v else None
  in
  match G.fold_vertex f g None with
  | Some v -> v
  | None -> raise Not_found

let reverse t =
  Lgr.info "Reversing %s graph" (string_of_phase t.phase);
  G.Mark.clear t.g;
  let phase = match t.phase with
    | Encode -> Decode
    | Decode -> Encode
    | Tag -> assert false
  in
  let inst_map = function
    | In1 -> Out1
    | In2 -> Out2
    | Out1 -> In1
    | Out2 -> In2
    | _ as i -> i
  in
  (* marked nodes are colored "red" and unmarked nodes are colored "blue" *)
  let mark v =
    (* XXX: This'll break for graphs with >= 1000 nodes. *)
    assert (G.Mark.get v <= 1000);
    G.Mark.set v (G.Mark.get v + 1000) in
  let is_marked v = G.Mark.get v > 1000 in
  let g = G.create () in
  let fchecks = function
    | Encode -> (fun inst v acc ->
        match inst with
        | Out1 | Out2 -> v :: acc
        | _ -> acc)
    | Decode -> (fun inst v acc ->
        match inst with
        | Fin1 | Fin2 -> v :: acc
        | _ -> acc)
    | Tag -> failwith "Fatal: Tag graph cannot be reversed"
  in
  let fchecks = fchecks phase in
  let f v (ctr, vs, checks) =
    let inst, _ = G.V.label v in
    let v' = G.V.create (inst_map inst, ref { typ = Bot; ctr = 0; (* array = [||] *) }) in
    let inst', _ = G.V.label v' in
    G.add_vertex g v';
    (* Give each vertex a unique mark, and make the two marks equal in both the
       old graph and the new graph, so that we can locate the same nodes between
       the two graphs. *)
    G.Mark.set v ctr;
    G.Mark.set v' ctr;
    let checks = fchecks inst' v' checks in
    match inst' with
    | In1 | In2 | Ini1 | Ini2 ->
      mark v; mark v'; (ctr + 1, vs, checks)
    | _ -> (ctr + 1, v :: vs, checks)
  in
  (* Create initial copy graph containing all the vertices but no edges yet. *)
  (* "vs" is the list we iterate over until empty or a fixed point is reached. *)
  (* "checks" are the vertices to be checked when verifying
     privacy/authenticity. *)
  let _, vs, checks = G.fold_vertex f t.g (1, [], []) in
  let fold acc v =
    let add_edge src dst = let e = G.E.create src () dst in G.add_edge_e g e in
    let add_edge_and_mark src dst v =
      add_edge src dst; mark dst; mark v in
    let neighbors = List.append (G.succ t.g v) (G.pred t.g v) |> List.to_array in
    let v' = find_vertex g (G.Mark.get v) in
    let inst, _ = G.V.label v in
    match inst with
    | In1 | In2 | Fin1 | Fin2 ->
      assert (Array.length neighbors = 1);
      let n = neighbors.(0) in
      let n' = find_vertex g (G.Mark.get n) in
      if is_marked n' then (add_edge_and_mark n' v' v; acc)
      else v :: acc
    | Tbc ->
      assert (Array.length neighbors = 2);
      let n1, n2 = neighbors.(0), neighbors.(1) in
      let n1' = find_vertex g (G.Mark.get n1) in
      let n2' = find_vertex g (G.Mark.get n2) in
      begin
        match is_marked n1', is_marked n2' with
        | true, true -> raise (Unreversable "TBC: Both vertices marked")
        | true, false -> add_edge_and_mark n1' v' v; acc
        | false, true -> add_edge_and_mark n2' v' v; acc
        | false, false -> v :: acc
      end
    | Dup ->
      if Array.length neighbors <> 3 then
        raise (Unreversable "DUP: Only one outgoing edge")
      else
        let n1, n2, n3 = neighbors.(0), neighbors.(1), neighbors.(2) in
        let n1' = find_vertex g (G.Mark.get n1) in
        let n2' = find_vertex g (G.Mark.get n2) in
        let n3' = find_vertex g (G.Mark.get n3) in
        begin
          match is_marked n1', is_marked n2', is_marked n3' with
          | true, true, true | true, true, false
          | true, false, true | false, true, true ->
            raise (Unreversable "DUP: More than one vertex marked")
          | true, false, false -> add_edge_and_mark n1' v' v; acc
          | false, true, false -> add_edge_and_mark n2' v' v; acc
          | false, false, true -> add_edge_and_mark n3' v' v; acc
          | false, false, false -> v :: acc
        end
    | Xor ->
      if Array.length neighbors <> 3 then
        raise (Unreversable "XOR: Only one incoming edge")
      else
        let n1, n2, n3 = neighbors.(0), neighbors.(1), neighbors.(2) in
        let n1' = find_vertex g (G.Mark.get n1) in
        let n2' = find_vertex g (G.Mark.get n2) in
        let n3' = find_vertex g (G.Mark.get n3) in
        begin
          match is_marked n1', is_marked n2', is_marked n3' with
          | true, true, true ->
            raise (Unreversable "XOR: More than two vertices marked")
          | true, true, false -> add_edge n1' v'; add_edge_and_mark n2' v' v; acc
          | true, false, true -> add_edge n1' v'; add_edge_and_mark n3' v' v; acc
          | false, true, true -> add_edge n2' v'; add_edge_and_mark n3' v' v; acc
          | _, _, _ -> v :: acc
        end
    | Out1 | Out2 | Ini1 | Ini2 -> assert false
  in
  (* Loop over list until empty or fixed point reached *)
  let rec loop l =
    if not (List.is_empty l) then begin
      Lgr.debug "Looping over %s" (List.to_string l ~f:string_of_v);
      let l' = List.fold l ~init:[] ~f:fold |> List.rev in
      if List.length l = List.length l' then
        raise (Unreversable "Reached fixed point but list not empty");
      loop l'
    end
  in
  try
    loop vs;
    Ok { g; phase; checks }
  with Unreversable err ->
    Or_error.errorf "Cannot derive encode graph: %s" err

let check_direction t =
  assert (t.phase = Encode);
  let t' = reverse t |> ok_exn in
  let tbcs = find_all_vertices_by_inst t.g Tbc in
  let f v =
    let p = G.pred t.g v |> List.hd_exn in
    let v' = find_vertex t'.g (G.Mark.get v) in
    let p' = G.pred t'.g v' |> List.hd_exn in
    assert (G.Mark.get v = G.Mark.get v');
    G.Mark.get p = G.Mark.get p'
  in
  List.for_all tbcs ~f

(* Checks that there exists paths between IN nodes and their associated OUT
   nodes *)
let check_paths t =
  let c = Check.create t.g in
  let f inst = find_vertex_by_inst t.g inst in
  if Check.check_path c (f In1) (f Out1)
     && Check.check_path c (f In2) (f Out2) then Ok ()
  else Or_error.errorf "Redundant graph: Paths don't check out"

(* Runs the Map function mapping nodes to (type, ctr) tuples *)
let map t types rand ~simple =
  Lgr.info "Checking %s %b" (List.to_string string_of_typ (Array.to_list types)) rand;
  let ntbc = find_all_vertices_by_inst t.g Tbc |> List.length in
  let i = ref 0 in
  let max_ctr = ref 0 in
  begin
    let f inst typ =
      let _, map = find_vertex_by_inst t.g inst |> G.V.label in
      let f = function
        | Rand -> max_ctr := 1; 1
        | _ -> 0 in
      map := { typ; ctr = f typ; (* array = Array.init ntbc (fun _ -> 0) *) }
    in
    let l =
      match t.phase with
      | Encode | Decode ->
        if simple then [| Ini1; In1; In2 |] else [| Ini1; Ini2; In1; In2 |]
      | Tag ->
        if simple then [| Ini1 |] else [| Ini1; Ini2 |]
    in
    Array.iter2_exn l types f
  end;
  let f v =
    let inst, _ = G.V.label v in
    match inst with
    | In1 | In2 | Ini1 | Ini2 -> ()
    | Fin1 | Fin2 | Out1 | Out2 | Dup ->
      let _, pmap = G.pred t.g v |> List.hd_exn |> G.V.label in
      let _, map = G.V.label v in
      map := !pmap
    | Tbc ->
      let _, pmap = G.pred t.g v |> List.hd_exn |> G.V.label in
      let _, map = G.V.label v in
      let array = Array.init ntbc (fun _ -> 0) in
      Array.set array !i 1;
      i := !i + 1;
      if rand || !pmap.typ = One || !pmap.typ = Rand then begin
        max_ctr := !max_ctr + 1;
        map := { typ = Rand; ctr = !max_ctr; (* array *) }
      end else map := { typ = !pmap.typ; ctr = !pmap.ctr; (* array *) }
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
      (* let array = xor_array p1map.array p2map.array in *)
      if (p1map.typ = Zero && p2map.typ = Zero)
      || (p1map.typ = Zero && p2map.typ = One)
      || (p1map.typ = One && p2map.typ = Zero) then
        let xor_types = function
        | One, One -> Bot
        | Zero, Zero -> Zero
        | Zero, One | One, Zero -> One
        | _ , _ -> assert false
        in
        map := { typ = xor_types (p1map.typ, p2map.typ); ctr = p1map.ctr; (* array *) }
      else if p1map.typ = Rand && p1map.ctr > p2map.ctr then
        map := { typ = Rand; ctr = p1map.ctr; (* array *) }
      else
        map := { typ = Bot; ctr = p1map.ctr; (* array *) }
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
  (* let f v = let _, map = G.V.label v in !map.array in *)
  let ctr v = let _, map = G.V.label v in !map.ctr in
  match t.checks with
  | a :: b :: [] ->
    (* if not (Array.equal (f a) (f b) (fun i j -> i = j)) then Ok () *)
    if ctr a <> ctr b then Ok ()
    else Or_error.errorf "Encode graph insecure: counters are equal"
  | _ -> assert false

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

let is_parallel t strong ~simple =
  Lgr.info "Checking parallelizability of %s graph" @@ string_of_phase t.phase;
  let maxcost = if strong then 1 else List.length (find_all_vertices_by_inst t.g Tbc) in
  let ini1, ini2 = ref 0, ref 0 in
  let pred v = G.pred t.g v |> List.hd_exn in
  let rec f v =
    let inst, _ = G.V.label v in
    match inst with
    | In1 | In2 -> 0
    | Ini1 -> !ini1
    | Ini2 -> !ini2
    | Dup -> f (pred v)
    | Xor ->
      let ps = G.pred t.g v in
      begin
        match List.length ps with
        | 1 -> f (List.hd_exn ps)
        | 2 ->
          let l = List.map ps ~f in
          max (List.nth_exn l 0) (List.nth_exn l 1)
        | _ -> assert false
      end
    | Tbc -> (f (pred v)) + 1
    | Fin1 | Fin2 | Out1 | Out2 -> f (pred v)
  in
  let run_f l val1 val2 =
    ini1 := val1; ini2 := val2;
    let r = List.map l ~f in
    if simple then r @ [0] else r
  in
  match t.phase with
  | Encode | Decode ->
    let l = [find_vertex_by_inst t.g Out1;
             find_vertex_by_inst t.g Out2;
             find_vertex_by_inst t.g Fin1] in
    let l = if simple then l else l @ [find_vertex_by_inst t.g Fin2] in
    let r = run_f l 0 0 in
    let new1, new2 = List.nth_exn r 2, List.nth_exn r 3 in
    if new1 > 0 || new2 > 0 then begin
      Lgr.debug "cost(FIN1) = %d, cost(FIN2) = %d" new1 new2;
      let r' = run_f l new1 new2 in
      let cost = max (List.nth_exn r 0) (List.nth_exn r 1) in
      let cost' = max (List.nth_exn r' 0) (List.nth_exn r' 1) in
      Lgr.debug "cost(Old) = %d, cost(New) = %d" cost cost';
      if cost <> cost' then false else cost <= maxcost
    end else
      let cost = max (List.nth_exn r 0) (List.nth_exn r 1) in
      cost <= maxcost
  | Tag -> assert false

let attack_vector t v =
  let d = match t.phase with Tag -> 2 | Encode | Decode -> 4 in
  let tbcs = find_all_vertices_by_inst t.g Tbc in
  let ntbcs = List.length tbcs in
  let empty = Array.init (d + ntbcs) (fun _ -> false) in
  let rec f v =
    let inst, _ = G.V.label v in
    let empty = Array.copy empty in
    match inst with
    | Ini1 -> Array.set empty 0 true; empty
    | Ini2 -> Array.set empty 1 true; empty (* simple graphs don't have this *)
    | In1 -> Array.set empty 2 true; empty
    | In2 -> Array.set empty 3 true; empty
    | Fin1 | Fin2 | Dup | Out1 | Out2 ->
      let v = G.pred t.g v |> List.hd_exn in f v
    | Tbc ->
      begin
        match List.findi tbcs (fun i v' -> v = v') with
        | Some (i, _) -> Array.set empty (d + i) true; empty
        | None -> assert false
      end
    | Xor ->
      let ps = G.pred t.g v in
      begin
        match List.length ps with
        | 1 -> empty
        | 2 -> xor_bit_array (f (List.nth_exn ps 0)) (f (List.nth_exn ps 1))
        | _ -> assert false
      end
  in
  let inst, _ = G.V.label v in
  let vector = f v in
  Lgr.debug "  Vector for %s: %s" (string_of_inst inst)
    (Array.to_list vector |> List.to_string ~f:Bool.to_string);
  vector

let attack_vector_by_inst t inst =
  attack_vector t (find_vertex_by_inst t.g inst)

let tbc_entries d array = Array.slice array d (Array.length array)

let is_attack_tag tenc tdec ttag ~simple =
  match is_secure ttag ~simple with
  | Ok () -> false              (* Secure schemes don't have attacks (duh) *)
  | Error _ -> false            (* XXX: not yet implemented! *)

let is_attack_enc tenc ~simple =
  match is_secure tenc ~simple with
  | Ok () -> false              (* Secure schemes don't have attacks (duh) *)
  | Error _ -> begin
      Lgr.info "Checking attack on Enc graph";
      let v1 = attack_vector_by_inst tenc Out1 |> tbc_entries 4 in
      let v2 = attack_vector_by_inst tenc Out2 |> tbc_entries 4 in
      Array.for_all v1 (fun b -> not b)
      || Array.for_all v2 (fun b -> not b)
      || Array.for_all2_exn v1 v2 (fun a b -> a = b)
    end

let is_attack_dec tenc tdec ~simple =
  match is_secure tdec ~simple with
  | Ok () -> false              (* Secure schemes don't have attacks (duh) *)
  | Error _ -> begin
      Lgr.info "Checking attack on Dec graph"; 
      let c = Check.create tdec.g in
      let check v1 v2 = Check.check_path c v1 v2 in
      let inst_to_v inst = find_vertex_by_inst tdec.g inst in
      let check_reachability () =
        (* If FIN1 cannot reach both INs, the scheme is insecure *)
        not (check (inst_to_v In1) (inst_to_v Fin1)
             && check (inst_to_v In2) (inst_to_v Fin1))
      in
      let check_dec_1 () =
        let check_ii c1 c2 u vs =
          let check t1 t2 =
            if xor_bit (and_bit c1 u.(0)) (and_bit c2 u.(1))
               = xor_bit (and_bit t1 u.(2)) (and_bit t2 u.(3)) then
              let f x = xor_bit (and_bit c1 x.(0)) (and_bit c2 x.(1))
                        = xor_bit (and_bit t1 x.(2)) (and_bit t2 x.(3)) in
              List.for_all vs f
            else false
          in
          check false false
          || check false true
          || check true false
          || check true true
        in
        let check_i () =
          let tbcs = find_all_vertices_by_inst tdec.g Tbc in
          let f inst =
            (* Get all TBC nodes reachable by 'inst' *)
            let tbcs = List.filter tbcs (fun v -> check v (inst_to_v inst)) in
            (* Get the attack vector of these nodes *)
            let vs = List.map tbcs (fun v -> attack_vector tdec v) in
            let check b1 b2 =
              let u = attack_vector_by_inst tdec Fin1 in
              let c1 = xor_bit (and_bit u.(2) b1) (and_bit u.(3) b2) in
              let c2 =
                if simple then false else
                  let w = attack_vector_by_inst tdec Fin2 in
                  xor_bit (and_bit w.(2) b1) (and_bit w.(3) b2)
              in
              check_ii c1 c2 u vs (* XXX: 'vs' should be only first FIN node *)
            in
            let f r1 r2 v = and_bit v.(2) r1 = and_bit v.(3) r2 in
            if List.for_all vs (fun v -> f true true v) then
              check true true
            else if List.for_all vs (fun v -> f false true v) then
              check false true
            else if List.for_all vs (fun v -> f true false v) then
              check true false
            else false
          in
          if simple then f Fin1 else f Fin1 || f Fin2
        in
        check_i ()
      in
      let check_dec_2 () =
        let v = attack_vector_by_inst tdec Fin1 in
        not v.(0) && not v.(1)
      in
      if check_reachability () then true
      else check_dec_1 () || check_dec_2 ()
    end

let is_attack tenc tdec ttag ~simple =
  is_attack_tag tenc tdec ttag ~simple
  || is_attack_enc tenc ~simple
  || is_attack_dec tenc tdec ~simple
