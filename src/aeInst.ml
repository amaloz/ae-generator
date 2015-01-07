open Core.Std
open AeOps

let n_in = function
  | Instruction Msg1 -> 0
  | Instruction Msg2 -> 0
  | Instruction Ini1 -> 0
  | Instruction Ini2 -> 0
  | Instruction Fin1 -> 1
  | Instruction Fin2 -> 1
  | Instruction Out1 -> 1
  | Instruction Out2 -> 1
  | Instruction Dup -> 1
  | Instruction Xor -> 2
  | Instruction Tbc -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let n_out = function
  | Instruction Msg1 -> 1
  | Instruction Msg2 -> 1
  | Instruction Ini1 -> 1
  | Instruction Ini2 -> 1
  | Instruction Fin1 -> 0
  | Instruction Fin2 -> 0
  | Instruction Out1 -> 0
  | Instruction Out2 -> 0
  | Instruction Dup -> 2
  | Instruction Xor -> 1
  | Instruction Tbc -> 1
  | StackInstruction Swap -> 2
  | StackInstruction Twoswap -> 3

let from_string s =
  match String.uppercase s with
  | "MSG1" -> Ok (Instruction Msg1)
  | "MSG2" -> Ok (Instruction Msg2)
  | "INI1" -> Ok (Instruction Ini1)
  | "INI2" -> Ok (Instruction Ini2)
  | "FIN1" -> Ok (Instruction Fin1)
  | "FIN2" -> Ok (Instruction Fin2)
  | "OUT1" -> Ok (Instruction Out1)
  | "OUT2" -> Ok (Instruction Out2)
  | "DUP" -> Ok (Instruction Dup)
  | "XOR" -> Ok (Instruction Xor)
  | "TBC" -> Ok (Instruction Tbc)
  | "SWAP" -> Ok (StackInstruction Swap)
  | "2SWAP" -> Ok (StackInstruction Twoswap)
  | "" -> Or_error.error_string "no instruction given"
  | _ as s -> Or_error.error_string (sprintf "unknown instruction '%s'" s)

let from_string_block s =
  List.map (String.split s ~on:' ') from_string |> Or_error.combine_errors

let block_length l =
  (* we do not count stack instructions as part of the block length *)
  let f acc = function
    | Instruction _ -> acc + 1
    | StackInstruction _ -> acc
  in
  List.fold_left l ~init:0 ~f:f

(* counts the number of modes of size 'size' in list 'found' *)
let count found size =
  List.count found (fun l -> block_length l = size)

let print_modes found maxsize =
  for i = 1 to maxsize do
    Printf.printf "# modes of length %d:\n" i;
    let l = List.filter found (fun block -> block_length block = i) in
    let l = List.map l (fun block -> string_of_op_list block) in
    let l = List.sort ~cmp:String.compare l in
    List.iter l (fun block -> Printf.printf "%s\n" block);
  done

let validate block phase =
  let eq x y = (Instruction x) = y in
  let check b s = if b then Ok () else Or_error.error_string s in
  let one inst =
    let c = List.count block (eq inst) in
    check (c = 1) (sprintf "Need exactly one %s instruction (%d found)"
                     (string_of_instruction inst) c)
  in
  let open Or_error.Monad_infix in
  begin
    match phase with
    | Encode | Decode ->
      let l = [Ini1; Ini2; Fin1; Fin2; Msg1; Msg2; Out1; Out2] in
      let l = List.map l one in
      Or_error.combine_errors_unit l
    | Tag ->
      let l = [Ini1; Ini2; Out1] in
      let l = List.map l one in
      Or_error.combine_errors_unit l
  end
  >>= fun () ->
  begin
    match phase with
    | Encode | Decode ->
      let f acc i = acc - n_in i + n_out i in
      if List.fold block ~init:0 ~f:f = 0 then
        Ok ()
      else
        Or_error.error_string (sprintf "Leftover items on stack in %s algorithm"
                                 (string_of_phase phase))
    | Tag -> Ok ()              (* TODO: verify Tag algorithm somehow *)
  end

let is_valid block =
  let eq x y = (Instruction x) = y in
  List.count block (eq Ini1) = 1
  && List.count block (eq Ini2) = 1
  && List.count block (eq Fin1) = 1
  && List.count block (eq Fin2) = 1
  && List.count block (eq Msg1) = 1
  && List.count block (eq Msg2) = 1
  && List.count block (eq Out1) = 1
  && List.count block (eq Out2) = 1
  && List.exists block (eq Tbc)

let is_pruneable i block =
  let cmp_prev i prev =
    let p i = prev = Instruction i in
    let ps i = prev = StackInstruction i in
    match i with
    | Instruction i ->
      begin
        match i with
        | Out1 | Out2 -> p Msg1 || p Msg2
        | Fin1 -> p Ini2
        | Fin2 -> p Ini1 
        | Tbc -> p Tbc
        | Xor -> p Dup
        | _ -> false
      end
    | StackInstruction i ->
      begin
        match i with
        | Swap -> p Dup || ps Swap
        | Twoswap -> ps Twoswap
      end
  in
  match block with
  | hd :: _ -> cmp_prev i hd
  | [] -> false
