open Core.Std
open Async.Std
open Async_parallel.Std
open AeOps

let minsize = 13

type synthInst =
  | Terminal

type synth_op =
  | Op of op
  | SynthInst of synthInst

let string_of_synth_inst = function
  | Terminal -> "TERM"

let n_in = function
  | Op op -> AeInst.n_in op
  | SynthInst Terminal -> 1

let n_out = function
  | Op op -> AeInst.n_out op
  | SynthInst Terminal -> 0

let n_diff = function
  | Op op -> AeInst.n_diff op
  | SynthInst Terminal -> -1

let is_valid block =
  let opeq x y = Op (Inst x) = y in
  let syntheq x y = SynthInst x = y in
  List.count block (opeq Ini1) = 1
  && List.count block (opeq Ini2) = 1
  && List.count block (opeq Msg1) = 1
  && List.count block (opeq Msg2) = 1
  && List.exists block (opeq Tbc)
  && List.count block (syntheq Terminal) = 4

let is_pruneable op block =
  let cmp_prev cur prev =
    let p cur = prev = Op (Inst cur) in
    let ps cur = prev = Op (StackInst cur) in
    match cur with
    | Op cur ->
      begin
        match cur with
        | Inst cur ->
          begin
            match cur with
            | Out1 | Out2 -> p Msg1 || p Msg2
            | Tbc -> p Tbc
            | Xor -> p Dup
            | _ -> false
          end
        | StackInst cur ->
          begin
            match cur with
            | Swap -> p Dup || ps Swap
            | Twoswap -> ps Twoswap
          end
      end
    | SynthInst Terminal -> false
  in
  match block with
  | hd :: _ -> cmp_prev op hd
  | [] -> false

let remove_dups blocks =
  let table = String.Table.create () ~size:1024 in
  let f block =
    let r = AeGraph.create block Decode |> ok_exn |> AeGraph.eval in
    Log.Global.info "Result = %s\n" r;
    match Hashtbl.add table ~key:r ~data:r with
    | `Duplicate -> false
    | `Ok -> true
  in
  List.filter blocks ~f

let string_of_synth_op = function
  | Op op -> string_of_op op
  | SynthInst Terminal -> "TERMINAL"

let string_of_synth_op_list l =
  List.to_string string_of_synth_op l

(* From: http://rosettacode.org/wiki/Permutations#OCaml *)
let rec permutations l =
  let n = List.length l in
  if n = 1 then [l] else
    let rec sub e = function
      | [] -> failwith "sub"
      | h :: t -> if h = e then t else h :: sub e t in
    let rec aux k =
      let e = List.nth_exn l k in
      let subperms = permutations (sub e l) in
      let t = List.map ~f:(fun a -> e :: a) subperms in
      if k < n-1 then List.rev_append t (aux (k+1)) else t in
    aux 0
let perms = permutations [Inst Fin1; Inst Fin2; Inst Out1; Inst Out2]

let process_decode block =
  let process block =
    Log.Global.info "Trying [%s]" (string_of_op_list block);
    let open Or_error.Monad_infix in
    AeGraph.create block Decode
    >>= fun decode ->
    AeGraph.is_secure decode
    >>= fun _ ->
    Log.Global.info "  Secure decode";
    AeGraph.derive_encode_graph decode
    >>= fun encode ->
    AeGraph.is_secure encode
    >>= fun _ ->
    Log.Global.info "Secure: [%s]\n%!" (string_of_op_list block);
    Ok block
  in
  let rec replace l l' =
    let f = function
      | Op op -> op
      | _ -> assert false
    in
    match l with
    | x :: xs -> if x = SynthInst Terminal then
        match l' with
        | y :: ys -> y :: (replace xs ys)
        | _ -> failwith "second list not of correct length"
      else f x :: (replace xs l')
    | [] -> []
  in
  Log.Global.info "Trying [%s]" (string_of_synth_op_list block);
  let l = List.map perms ~f:(fun perm -> replace block perm |> process) in
  let l = List.filter_map l ~f:(function
      | Ok block -> Some block
      | Error e -> Log.Global.info "%s" (Error.to_string_hum e); None)
  in
  l

let gen ?(all=false) ?(print=false) size phase =
  (* let f l = *)
  (*   List.iter ~f:(fun op -> printf " %s" (string_of_op op)) l; *)
  (*   printf "\n%!" in *)
  (* List.iter ~f perms; *)
  let ops = [
    Op (Inst Msg1); Op (Inst Msg2);
    Op (Inst Ini1); Op (Inst Ini2);
    Op (Inst Dup); Op(Inst Xor); Op(Inst Tbc);
    Op (StackInst Swap); Op (StackInst Twoswap);
    SynthInst Terminal
  ] in
  let initial = [
    Op (Inst Ini1);
    Op (Inst Ini2);
    Op (Inst Msg1);
    Op (Inst Msg2)
  ] in
  let rec fold depth ninputs block counts acc op =
    (* n contains the number of available inputs after 'op' is used *)
    let n = ninputs + (n_diff op) in
    if n >= 0 then
      let count = List.Assoc.find counts op in
      let skip = match count with
        | Some c -> c = 0
        | None -> false
      in
      if true (* not skip *) then
        if n_in op <= ninputs (* && not (is_pruneable op block) *) then
          let counts = match count with
            | Some c -> List.Assoc.add counts op (c - 1)
            | None -> counts
          in
          loop (depth - 1) n (op :: block) counts acc
        else acc
      else acc
    else acc
  and loop depth ninputs block counts acc =
    match depth with
    | 0 ->
      let block = List.rev block in
      if ninputs = 0 (* && is_valid block *) then
        match phase with
        | Decode -> process_decode block |> List.append acc
        | Encode -> failwith "cannot generate encode graphs"
        | Tag -> failwith "generating tag graphs not yet implemented"
      else
        acc
    | _ when depth > 0 ->
      if depth = size - 2 then begin
        Log.Global.debug "Finding modes starting with %s..."
          (List.map (List.rev block) string_of_synth_op |> String.concat ~sep:" ");
        let start = Time.now () in
        let blocks = List.fold ops ~init:acc
            ~f:(fold depth ninputs block counts) in
        let stop = Time.now () in
        Log.Global.debug "  Took: %s" (Time.diff stop start |> Time.Span.to_string);
        blocks
      end
      else
        List.fold ops ~init:acc ~f:(fold depth ninputs block counts)
    | _ -> acc
  in
  let counts = [
    Op (Inst Ini1), 1;
    Op (Inst Ini2), 1;
    Op (Inst Msg1), 1;
    Op (Inst Msg2), 1;
    (* XXX: Limiting number of TBCs to <= 4 *)
    Op (Inst Tbc), 4;
    SynthInst Terminal, 4;
  ] in
  let found = ref [] in
  let worker h =
    Pipe.iter_without_pushback (Hub.listen_simple h) ~f:(fun (id, op) ->
        Log.Global.debug "Got op %s" (string_of_synth_op op);
        let blocks = fold size 0 [] counts [] op in
        Hub.send h id blocks)
    >>| fun () -> `Done
  in
  Log.Global.debug "Generating modes of size %d" size;
  Deferred.List.iter ~how:`Parallel initial ~f:(fun op ->
      Parallel.spawn ~where:Parallel.round_robin worker
      >>= fun (c, res) ->
      Channel.write c op;
      Channel.read c
      >>| fun blocks ->
      found := List.append blocks !found
    )
  >>| fun () -> remove_dups !found |> return
  >>| fun found ->
  if print then
    AeInst.print_modes found size;
  printf "# found modes: %d\n" (List.length found);
  begin
    if all then
      let minsize = min minsize size in
      for i = minsize to size do
        printf "# modes of size %d = %d\n%!" i (AeInst.count found i)
      done
  end;
  Shutdown.shutdown 0
