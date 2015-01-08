open Core.Std
open AeOps

let minsize = 13

let exists_decode tbl decode =
  let r = AeGraph.eval decode in
  Log.info "Result = %s\n" r;
  match Hashtbl.add tbl ~key:r ~data:r with
  | `Duplicate -> true
  | `Ok -> false

let process_decode block tbl =
  Log.info "Trying [%s]" (string_of_op_list block);
  let decode = AeGraph.create block Decode |> ok_exn in
  if AeGraph.is_secure decode then begin
    Log.info "  Secure decode";
    let open Or_error.Monad_infix in
    AeGraph.derive_encode_graph decode
    >>= fun encode ->
    if AeGraph.is_secure encode then begin
      Log.info "Secure: [%s]\n%!" (string_of_op_list block);
      if exists_decode tbl decode then
        Or_error.error_string "Scheme already found"
      else
        Ok ()
    end else Or_error.error_string "Encode graph insecure"
  end else Or_error.error_string "Unable to derive encode graph"

let gen ?(all=false) size tbl phase =
  let ops = List.map all_ops (fun i -> AeInst.from_string i)
            |> Or_error.combine_errors |> ok_exn in
  let initial = [
    Instruction Ini1;
    Instruction Ini2;
    Instruction Msg1;
    Instruction Msg2
  ] in
  let _gen depth counts =
    Log.debug "Generating modes of size %d" depth;
    let rec fold depth ninputs block counts acc i =
      (* n contains the number of available inputs after 'i' is used *)
      let n = ninputs + (AeInst.n_diff i) in
      if n >= 0 then
        let count = List.Assoc.find counts i in
        let skip = match count with
          | Some c -> c = 0
          | None -> false
        in
        if not skip then
          if AeInst.n_in i <= ninputs && not (AeInst.is_pruneable i block) then
            let counts = match count with
              | Some c -> List.Assoc.add counts i (c - 1)
              | None -> counts
            in
            loop (depth - 1) n (i :: block) counts acc
          else acc
        else acc
      else acc
    and loop depth ninputs block counts acc =
      match depth with
      | 0 ->
        let block = List.rev block in
        if ninputs = 0 && AeInst.is_valid block then
          match phase with
          | Decode -> begin
              match process_decode block tbl with
              | Ok _ -> block :: acc
              | Error err -> Log.info "%s" (Error.to_string_hum err); acc
            end
          | Encode -> failwith "cannot generate encode graphs"
          | Tag -> failwith "generating tag graphs not yet implemented"
        else
          acc
      | _ when depth > 0 ->
        if depth = size - 2 then begin
          printf "Finding modes starting with %s...\n%!"
            (List.map (List.rev block) string_of_op |> String.concat ~sep:" ");
          let start = Time.now () in
          let blocks = List.fold ops ~init:acc
              ~f:(fold depth ninputs block counts) in
          let stop = Time.now () in
          printf "  Took: %s\n%!" (Time.diff stop start |> Time.Span.to_string);
          blocks
        end
        else
          List.fold ops ~init:acc ~f:(fold depth ninputs block counts)
      | _ -> acc
    in
    List.fold initial ~init:[] ~f:(fold depth 0 [] counts)
  in
  let counts = [
    Instruction Ini1, 1;
    Instruction Ini2, 1;
    Instruction Fin1, 1;
    Instruction Fin2, 1;
    Instruction Msg1, 1;
    Instruction Msg2, 1;
    Instruction Out1, 1;
    Instruction Out2, 1;
    (* XXX: Limiting number of TBCs to <= 4 *)
    Instruction Tbc, 4;
  ] in
  let f found size =
    let blocks = _gen size counts in
    List.append found blocks
  in
  if all then
    let minsize = min minsize size in
    let sizes = List.range minsize (size + 1) in
    List.fold_left sizes ~init:[] ~f:f
  else
    f [] size
