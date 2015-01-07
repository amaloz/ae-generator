open Core.Std
open AeOps

let exists_decode tbl decode =
  let r = AeGraph.eval decode in
  Log.info "Result = %s\n" r;
  match Hashtbl.add tbl ~key:r ~data:r with
  | `Duplicate -> true
  | `Ok -> false

let gen ?(all=false) size insts tbl phase =
  let _gen depth counts tbl phase =
    Log.debug "Generating modes of size %d" depth;
    let blocks = ref [] in
    let process_decode block =
      Log.info "Trying [%s]" (string_of_op_list block);
      let decode = AeGraph.create block phase |> ok_exn in
      if AeGraph.is_secure decode then
        begin
          Log.info "  Secure decode";
          match AeGraph.derive_encode_graph decode with
          | Ok encode ->
            if AeGraph.is_secure encode then
              begin
                Log.info "  It works!";
                printf "Secure: [%s]\n%!" (string_of_op_list block);
                if exists_decode tbl decode then
                  Log.info "  Already exists..."
                else
                  blocks := block :: !blocks
              end
            else
              Log.info "  Encode graph insecure";
          | Error _ ->
            Log.info "  Unable to derive encode graph"
        end
    in
    let rec iter depth ninputs block counts i =
      let count = List.Assoc.find counts i in
      let skip = match count with
        | Some c -> c = 0
        | None -> false
      in
      if not skip then
        (* n contains the number of available inputs after 'i' is used *)
        let n = ninputs - (AeInst.n_in i) + (AeInst.n_out i) in
        (* only loop if 'i' is a valid instruction to use here *)
        if n >= 0 
        && AeInst.n_in i <= ninputs
        && not (AeInst.is_pruneable i block) then begin
          let counts = match count with
            | Some c -> List.Assoc.add counts i (c - 1)
            | None -> counts
          in
          loop (depth - 1) n (i :: block) counts
        end
    and loop depth ninputs block counts =
      match depth with
      | 0 ->
        (* Log.info "Hit bottom... [%s]" (string_of_op_list (List.rev block)); *)
        let block = List.rev block in
        if ninputs = 0 && AeInst.is_valid block then
          begin
            match phase with
            | Decode -> process_decode block
            | Encode -> failwith "cannot generate encode graphs"
            | Tag -> failwith "generating tag graphs not yet implemented"
          end
      | _ when depth > 0 ->
        List.iter insts (iter depth ninputs block counts)
      | _ -> ()
    in
    List.iter insts (iter depth 0 [] counts);
    !blocks
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
    Instruction Tbc, 4;
  ] in
  let f found size =
    let blocks = _gen size counts tbl phase in
    List.append found blocks
  in
  if all then
    let sizes = List.range 10 (size + 1) in
    List.fold_left sizes ~init:[] ~f:f
  else
    f [] size
  
  
