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
  Op (Inst Tbc), 4;             (* Allow a max number of 4 TBC nodes *)
  SynthInst Start, 4;
  SynthInst Terminal, 4;
]
let counts_simple = [
  Op (Inst Tbc), 4;             (* Allow a max number of 4 TBC nodes *)
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
  let msg1, msg2 = AeGraph.msg1, AeGraph.msg2 in
  let swap = function
    | a :: b :: xs -> b :: a :: xs
    | _ -> assert false in
  let table = String.Table.create () ~size:1024 in
  let f block =
    let encode = AeGraph.create block Encode |> ok_exn in
    let r = AeGraph.eval encode ~simple ~msg1 ~msg2 in
    let r' = swap r in
    let s = AeGraph.eval encode ~simple ~msg1:msg2 ~msg2:msg1 in
    let s' = swap s in
    Lgr.info "Decode: %s" (string_of_op_list block);
    let tmp = String.Table.create () ~size:1024 in
    let add l =
      let s = String.concat ~sep:" " l in
      Lgr.info "%s" s;
      ignore (Hashtbl.add tmp ~key:s ~data:s) in
    add r; add r'; add s; add s';
    let b = Hashtbl.fold tmp ~init:true ~f:(fun ~key ~data c ->
        if c then
          match Hashtbl.add table ~key ~data with
          | `Duplicate -> false
          | `Ok -> true
        else c
      ) in
    Lgr.info "Duplicate? %s" (if b then "No" else "Yes");
    b
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

let start_perms = permutations [Inst Ini1; Inst Ini2; Inst In1; Inst In2]
let start_perms_simple = permutations [Inst Ini1; Inst In1; Inst In2]

let term_perms = permutations [Inst Fin1; Inst Fin2; Inst Out1; Inst Out2]
let term_perms_simple = permutations [Inst Fin1; Inst Out1; Inst Out2]

let try_count = ref 0

let process block ~simple =
  let process block =
    Lgr.info "Trying %s" (string_of_op_list block);
    let open Or_error.Monad_infix in
    AeGraph.create block Decode      >>= fun decode ->
    AeGraph.check_paths decode       >>= fun () ->
    AeGraph.reverse decode ~simple   >>= fun encode ->
    AeGraph.is_secure encode ~simple >>= fun () ->
    AeGraph.is_secure decode ~simple >>| fun () ->
    (* AeGraph.create block Encode      >>= fun encode -> *)
    (* AeGraph.check_paths encode       >>= fun () -> *)
    (* AeGraph.is_secure encode ~simple >>= fun () -> *)
    (* AeGraph.reverse encode ~simple   >>= fun decode -> *)
    (* AeGraph.check_paths decode       >>= fun () -> *)
    (* AeGraph.is_secure decode ~simple >>| fun () -> *)
    Lgr.info "Secure: %s" (string_of_op_list block);
    printf "%s\n%!" (string_of_op_list block);
    block
  in
  (* Replace terminal nodes with leaf nodes *)
  let rec replace ~block ~perm ~typ =
    match block with
    | x :: xs ->
      if x = SynthInst typ then
        match perm with
        | y :: ys -> Op y :: (replace ~block:xs ~perm:ys ~typ)
        | _ -> assert false
      else x :: (replace ~block:xs ~perm ~typ)
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
    let block = replace ~block ~perm ~typ:Terminal in
    let f perm = replace ~block ~perm ~typ:Start |> strip |> process in
    List.map starts ~f
  in
  let l = List.map terms ~f |> List.join in
  List.filter_map l ~f:(function
      | Ok block -> Some block
      | Error _ -> None)

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
      process block ~simple |> List.append acc
    else
      acc
  | _ when depth > 0 ->
    List.fold ops ~init:acc ~f:(fold ~simple ~maxsize ~depth ~ninputs ~block ~counts)
  | _ -> acc

let gen ~simple ~print size =
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
  if print then
    List.iter found (fun block -> printf "%s\n%!" (string_of_op_list block))
