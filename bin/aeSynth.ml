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

let try_count = ref 0

let is_valid ~simple block =
  let opeq x y = Op (Inst x) = y in
  let syntheq x y = SynthInst x = y in
  List.exists block ~f:(opeq Tbc)
  && List.exists block ~f:(opeq Xor)
  && List.exists block ~f:(opeq Dup)
  && List.count block ~f:(syntheq Start) = (if simple then 3 else 4)
  && List.count block ~f:(syntheq Terminal) = (if simple then 3 else 4)

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

let remove_dups blocks ~use_enc ~simple =
  let get_encode =
    if use_enc then fun block -> AeGraph.create block Encode |> ok_exn
    else fun block ->
      let decode = AeGraph.create block Decode |> ok_exn in
      AeGraph.reverse decode |> ok_exn
  in
  let msg1, msg2 = AeGraph.msg1, AeGraph.msg2 in
  let swap = function
    | a :: b :: xs -> b :: a :: xs
    | _ -> assert false in
  let table = String.Table.create () ~size:1024 in
  let f block =
    let encode = get_encode block in
    let r = AeGraph.eval encode ~simple ~msg1 ~msg2 in
    let s = AeGraph.eval encode ~simple ~msg1:msg2 ~msg2:msg1 in
    let tmp = String.Table.create () ~size:1024 in
    let add l =
      let s = String.concat ~sep:" " l in
      ignore (Hashtbl.add tmp ~key:s ~data:s) in
    add r; add (swap r); add s; add (swap s);
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
  List.to_string ~f:string_of_synth_op l

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

let fprocess ~use_enc ~simple =
  let open Or_error.Monad_infix in
  if use_enc then
    fun block ->
      AeGraph.create block Encode      >>= fun encode ->
      AeGraph.check_paths encode       >>= fun () ->
      AeGraph.reverse encode           >>= fun decode ->
      AeGraph.is_secure encode ~simple >>= fun () ->
      AeGraph.is_secure decode ~simple >>= fun () ->
      Ok (encode, decode)
  else
    fun block ->
      AeGraph.create block Decode      >>= fun decode ->
      AeGraph.check_paths decode       >>= fun () ->
      AeGraph.is_secure decode ~simple >>= fun () ->
      AeGraph.reverse decode           >>= fun encode ->
      AeGraph.is_secure encode ~simple >>= fun () ->
      Ok (encode, decode)

let fprocess' ~use_enc ~simple: _ =
  (* Don't actually check security here *)
  let open Or_error.Monad_infix in
  if use_enc then
    fun block ->
      AeGraph.create block Encode      >>= fun encode ->
      AeGraph.check_paths encode       >>= fun () ->
      AeGraph.reverse encode           >>= fun decode ->
      Ok (encode, decode)
  else
    fun block ->
      AeGraph.create block Decode      >>= fun decode ->
      AeGraph.check_paths decode       >>= fun () ->
      AeGraph.reverse decode           >>= fun encode ->
      Ok (encode, decode)

let process block ~fprocess ~simple ~attack =
  let process block =
    Lgr.info "Trying %s" (string_of_op_list block);
    try_count := !try_count + 1;
    let open Or_error.Monad_infix in
    fprocess block >>| fun (_, _) ->
    Lgr.info "Secure: %s" (string_of_op_list block);
    (* Need this for synthesis if we kill execution before finishing *)
    printf "%s\n%!" (string_of_op_list block);
    block
  in
  let process' block =
    Lgr.info "Trying %s" (string_of_op_list block);
    try_count := !try_count + 1;
    let open Or_error.Monad_infix in
    fprocess block >>= fun (encode, decode) ->
    (* Only reject if we can find an attack *)
    begin
      if AeGraph.is_attack_enc encode ~simple
         || AeGraph.is_attack_dec encode decode ~simple then
        Or_error.errorf "Attack found"
      else Ok ()
    end >>| fun () -> block
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
  let starts, terms =
    if simple then start_perms_simple, term_perms_simple
    else start_perms, term_perms
  in
  let process = if attack then process' else process in
  let f perm =
    let block = replace ~block ~perm ~typ:Terminal in
    let f perm = replace ~block ~perm ~typ:Start |> strip |> process in
    List.map starts ~f
  in
  let l = List.map terms ~f |> List.join in
  List.filter_map l ~f:(function
      | Ok block -> Some block
      | Error _ -> None)

let rec fold acc op ~fprocess ~simple ~maxsize ~depth ~ninputs ~block ~counts ~attack =
  (* n contains the number of available inputs after 'op' is used *)
  let n = ninputs - n_in op + n_out op in
  if n >= 0 then
    let count = List.Assoc.find counts ~equal:(=) op in
    let skip = match count with
      | Some c -> c = 0
      | None -> false
    in
    if not skip then
      if n_in op <= ninputs then
        if not (is_pruneable op block) then
          let counts = match count with
            | Some c -> List.Assoc.add counts ~equal:(=) op (c - 1)
            | None -> counts
          in
          loop ~fprocess ~simple ~maxsize ~depth:(depth - 1) ~ninputs:n
            ~block:(op :: block) ~counts ~attack acc
        else acc
      else acc
    else acc
  else acc
and loop acc ~fprocess ~simple ~maxsize ~depth ~ninputs ~block ~counts ~attack =
  match depth with
  | 0 ->
    let block = List.rev block in
    if ninputs = 0 && is_valid ~simple block then
      process block ~fprocess ~simple ~attack |> List.append acc
    else
      acc
  | _ when depth > 0 ->
    List.fold ops ~init:acc
      ~f:(fold ~fprocess ~simple ~maxsize ~depth ~ninputs ~block ~counts ~attack)
  | _ -> acc

let synth size ~use_enc ~simple ~print ~attack =
  let counts = if simple then counts_simple else counts in
  let fprocess = if attack
    then fprocess' ~use_enc ~simple
    else fprocess ~use_enc ~simple in
  let f acc op =
    let blocks = fold ~fprocess ~simple ~maxsize:size ~depth:size ~ninputs:0
        ~block:[] ~counts ~attack [] op in
    List.append blocks acc
  in
  let found = List.fold initial ~init:[] ~f in
  printf "# Tried:  %d\n%!" !try_count;
  if attack then
    printf "# No attacks: %d\n%!" (List.length found)
  else
    printf "# Secure: %d\n%!" (List.length found);
  let found = remove_dups ~use_enc ~simple found in
  printf "# Unique: %d\n%!" (List.length found);
  if print then
    List.iter found ~f:(fun block -> printf "%s\n%!" (string_of_op_list block))
