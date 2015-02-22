open AeInclude

type synthInst =
  | Start
  | Terminal

type synth_op =
  | Op of op
  | SynthInst of synthInst

let string_of_synth_inst = function
  | Start -> "START"
  | Terminal -> "TERM"

let n_in = function
  | Op op -> AeInst.n_in op
  | SynthInst Start -> 0
  | SynthInst Terminal -> 1

let n_out = function
  | Op op -> AeInst.n_out op
  | SynthInst Start -> 1
  | SynthInst Terminal -> 0

let initial = [
  SynthInst Start
]

let counts = [
  Op (Inst Tbc), 4;
  SynthInst Start, 4;
  SynthInst Terminal, 4;
]
let counts_simple = [
  Op (Inst Tbc), 4;
  SynthInst Start, 3;
  SynthInst Terminal, 3;
]

(* everything but the leaf operations *)
let ops = List.append initial [
  Op (Inst Dup); Op(Inst Xor); Op(Inst Tbc);
  Op (StackInst Swap); Op (StackInst Twoswap);
  SynthInst Terminal
]

let is_valid ~simple block =
  let opeq x y = Op (Inst x) = y in
  let syntheq x y = SynthInst x = y in
  List.exists block (opeq Tbc)
  && List.exists block (opeq Xor)
  && List.exists block (opeq Dup)
  && List.count block (syntheq Start) = (if simple then 3 else 4)
  && List.count block (syntheq Terminal) = (if simple then 3 else 4)

let is_pruneable op block =
  let cmp_prev cur prev =
    let p cur = prev = Op (Inst cur) in
    let ps cur = prev = Op (StackInst cur) in
    match cur with
    | Op (Inst cur) -> begin
        match cur with
        | Tbc -> p Tbc
        | Xor -> p Dup
        | _ -> false
      end
    | Op (StackInst cur) -> begin
        match cur with
        | Swap -> p Dup || ps Swap
        | Twoswap -> ps Twoswap
      end
    | SynthInst _ -> false
  in
  match block with
  | hd :: _ -> cmp_prev op hd
  | [] -> false

let remove_dups blocks ~simple =
  let table = String.Table.create () ~size:1024 in
  let f block =
    let decode = AeGraph.create block Decode |> ok_exn in
    let encode = AeGraph.derive_encode_graph decode |> ok_exn in
    let r = AeGraph.eval ~simple decode in
    let r' = AeGraph.eval ~simple encode in
    let r = String.concat ~sep:" " [r; r'] in
    Lgr.info "Decode: %s" (string_of_op_list block);
    Lgr.info "Result: %s" r;
    match Hashtbl.add table ~key:r ~data:r with
    | `Duplicate -> false
    | `Ok -> true
  in
  List.filter blocks ~f

let string_of_synth_op = function
  | Op op -> string_of_op op
  | SynthInst inst -> string_of_synth_inst inst

let string_of_synth_op_list l =
  List.to_string string_of_synth_op l

(* From: http://rosettacode.org/wiki/Permutations#OCaml *)
let rec permutations l =
  let n = List.length l in
  if n = 1 then [l] else
    let rec sub e = function
      | [] -> assert false
      | h :: t -> if h = e then t else h :: sub e t in
    let rec aux k =
      let e = List.nth_exn l k in
      let subperms = permutations (sub e l) in
      let t = List.map ~f:(fun a -> e :: a) subperms in
      if k < n-1 then List.rev_append t (aux (k + 1)) else t in
    aux 0

let start_perms = permutations [Inst Ini1; Inst Ini2; Inst Msg1; Inst Msg2]
let start_perms_simple = permutations [Inst Ini1; Inst Msg1; Inst Msg2]

let term_perms = permutations [Inst Fin1; Inst Fin2; Inst Out1; Inst Out2]
let term_perms_simple = permutations [Inst Fin1; Inst Out1; Inst Out2]

let try_count = ref 0

let process_decode block ~simple =
  let process block =
    Lgr.info "Trying %s" (string_of_op_list block);
    let open Or_error.Monad_infix in
    AeGraph.create block Decode        >>= fun decode ->
    AeGraph.derive_encode_graph decode >>= fun encode ->
    AeGraph.is_secure encode ~simple   >>= fun _ ->
    AeGraph.is_secure decode ~simple   >>| fun _ ->
    Lgr.info "Secure: %s" (string_of_op_list block);
    printf "%s\n%!" (string_of_op_list block);
    block
  in
  (* Replace terminal nodes with leaf nodes *)
  let rec replace l l' ~typ =
    match l with
    | x :: xs -> if x = SynthInst typ then
        match l' with
        | y :: ys -> Op y :: (replace xs ys ~typ)
        | _ -> assert false
      else x :: (replace xs l' ~typ)
    | [] -> []
  in
  let rec strip = function
    | x :: xs ->
      let f = function Op op -> op | _ -> assert false in
      f x :: (strip xs)
    | [] -> []
  in
  Lgr.info "Trying [%s]" (string_of_synth_op_list block);
  try_count := !try_count + 1;
  let starts, terms =
    if simple then start_perms_simple, term_perms_simple
    else start_perms, term_perms
  in
  let f perm =
    let block = replace block perm Terminal in
    let f perm = replace block perm Start |> strip |> process in
    List.map starts ~f
  in
  let l = List.map terms ~f |> List.join in
  List.filter_map l ~f:(function
      | Ok block -> Some block
      | Error e -> None)

(* module Worker = struct *)
(*   module T = struct *)
(*     type 'worker functions = { *)
(*       gen: ('worker, int * synth_op, op list list) Parallel.Function.t *)
(*     } *)

(*     type init_arg = unit with bin_io *)
(*     let init = return *)

(*     module Functions(C:Parallel.Creator) = struct *)
(*       let rec fold size depth ninputs block counts acc op = *)
(*         (\* n contains the number of available inputs after 'op' is used *\) *)
(*         let n = ninputs - n_in op + n_out op in *)
(*         if n >= 0 then *)
(*           let count = List.Assoc.find counts op in *)
(*           let skip = match count with *)
(*             | Some c -> c = 0 *)
(*             | None -> false *)
(*           in *)
(*           if not skip then *)
(*             if n_in op <= ninputs && not (is_pruneable op block) then *)
(*               let counts = match count with *)
(*                 | Some c -> List.Assoc.add counts op (c - 1) *)
(*                 | None -> counts *)
(*               in *)
(*               loop size (depth - 1) n (op :: block) counts acc *)
(*             else acc *)
(*           else acc *)
(*         else acc *)
(*       and loop size depth ninputs block counts acc = *)
(*         match depth with *)
(*         | 0 -> *)
(*           let block = List.rev block in *)
(*           if ninputs = 0 && is_valid block then *)
(*             match Decode (\* phase *\) with *)
(*             | Decode -> process_decode block |> List.append acc *)
(*             | Encode -> failwith "cannot generate encode graphs" *)
(*             | Tag -> failwith "generating tag graphs not yet implemented" *)
(*           else *)
(*             acc *)
(*         | _ when depth > 0 -> *)
(*           if depth = size - 2 then begin *)
(*             Log.Global.debug "Finding modes starting with %s..." *)
(*               (List.map (List.rev block) string_of_synth_op |> String.concat ~sep:" "); *)
(*             let start = Time.now () in *)
(*             let blocks = List.fold ops ~init:acc *)
(*                 ~f:(fold size depth ninputs block counts) in *)
(*             let stop = Time.now () in *)
(*             Log.Global.debug "  Took: %s" (Time.diff stop start |> Time.Span.to_string); *)
(*             blocks *)
(*           end *)
(*           else *)
(*             List.fold ops ~init:acc ~f:(fold size depth ninputs block counts) *)
(*         | _ -> acc *)

(*       type gen_arg = int * synth_op with bin_io *)
(*       let gen_impl (size, op) = fold size size 0 [] counts [] op |> return *)
(*       let gen = C.create_rpc ~f:gen_impl ~bin_input:bin_gen_arg *)
(*           ~bin_output:(List.bin_t (List.bin_t bin_op)) () *)

(*       let functions = { gen } *)
(*     end *)
(*   end *)
(*   include Parallel.Make_worker(T) *)
(* end *)

let rec fold ~simple ~maxsize ~depth ~ninputs ~block ~counts acc op =
  (* n contains the number of available inputs after 'op' is used *)
  let n = ninputs - n_in op + n_out op in
  if n >= 0 then
    let count = List.Assoc.find counts op in
    let skip = match count with
      | Some c -> c = 0
      | None -> false
    in
    if not skip then
      if n_in op <= ninputs && not (is_pruneable op block) then
        let counts = match count with
          | Some c -> List.Assoc.add counts op (c - 1)
          | None -> counts
        in
        loop ~simple ~maxsize ~depth:(depth - 1) ~ninputs:n ~block:(op :: block) ~counts acc
      else acc
    else acc
  else acc
and loop ~simple ~maxsize ~depth ~ninputs ~block ~counts acc =
  match depth with
  | 0 ->
    let block = List.rev block in
    if ninputs = 0 && is_valid ~simple block then
      process_decode block ~simple |> List.append acc
    else
      acc
  | _ when depth > 0 ->
    List.fold ops ~init:acc ~f:(fold ~simple ~maxsize ~depth ~ninputs ~block ~counts)
  | _ -> acc

let gen ?(print=false) ?(simple=false) size phase =
  assert (phase = Decode);
  (* let worker h = *)
  (*   Pipe.iter_without_pushback (Hub.listen_simple h) ~f:(fun (id, op) -> *)
  (*       Log.Global.debug "Got op %s" (string_of_synth_op op); *)
  (*       let blocks = fold size size 0 [] counts [] op in *)
  (*       Hub.send h id blocks) *)
  (*   >>| fun () -> `Done *)
  (* in *)
  Lgr.info "Generating %s modes of size %d"
    (if simple then "simple" else "normal") size;
  let counts = if simple then counts_simple else counts in
  let f acc op = 
    let blocks = fold ~simple ~maxsize:size ~depth:size ~ninputs:0 ~block:[]
        ~counts [] op in
    List.append blocks acc
  in
  let found = List.fold initial ~init:[] ~f in
  printf "# Tried:  %d\n%!" !try_count;
  printf "# Secure: %d\n%!" (List.length found);
  let found = remove_dups ~simple found in
  printf "# Unique: %d\n%!" (List.length found);
  (* Deferred.List.iter (\* ~how:`Parallel *\) initial ~f:(fun op -> *)
  (*     (\* Worker.spawn_exn () ~on_failure:Error.raise *\) *)
  (*     (\* >>= fun worker -> *\) *)
  (*     (\* Worker.run_exn worker ~f:Worker.functions.gen ~arg:(size, op) *\) *)
  (*     (\* >>| fun blocks -> *\) *)
  (*     (\* found := List.append blocks !found *\) *)
  (*     let blocks = fold size size 0 [] counts [] op in *)
  (*     found := List.append blocks !found; *)
  (*     Deferred.unit *)
  (*     (\* Parallel.spawn ~where:Parallel.round_robin worker >>= fun (c, _) -> *\) *)
  (*     (\* Channel.write c op; Channel.read c >>| fun blocks -> *\) *)
  (*     (\* found := List.append blocks !found *\) *)
  (*   ) >>= fun () -> *)
  (* return (remove_dups !found) >>| fun found -> *)
  if print then
    List.iter found (fun block -> printf "%s\n%!" (string_of_op_list block));
  shutdown ()
