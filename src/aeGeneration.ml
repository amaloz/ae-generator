open Core.Std
open AeOps

(* Check if a mode already exists in duplicate items table 'tbl'. *)
let exists ?(keep_dups=false) tbl g =
  if keep_dups then false
  else
    let r = AeGraph.eval g in
    Log.info "Result = %s\n" r;
    match Hashtbl.add tbl ~key:r ~data:r with
    | `Duplicate -> true
    | `Ok -> false

let gen ?(all=false) size insts tbl phase =
  let _gen depth counts tbl phase =
    Log.debug "Generating modes of size %d" depth;
    let blocks = ref [] in
    let process block =
      Log.info "Trying [%s]" (string_of_op_list block);
      let g = AeGraph.create block phase in
      if AeGraph.is_secure g then
        begin
          Log.info "It works!";
          printf "[%s]\n%!" (string_of_op_list block);
          exit 1;
          if exists tbl g then
            Log.info "already exists..."
          else
            blocks := block :: !blocks
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
          process block
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
  
  
