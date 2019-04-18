open AeInclude

let n_in = function
  | Inst In1 -> 0
  | Inst In2 -> 0
  | Inst Ini1 -> 0
  | Inst Ini2 -> 0
  | Inst Fin1 -> 1
  | Inst Fin2 -> 1
  | Inst Out1 -> 1
  | Inst Out2 -> 1
  | Inst Dup -> 1
  | Inst Xor -> 2
  | Inst Tbc -> 1
  | StackInst Swap -> 2
  | StackInst Twoswap -> 3

let n_out = function
  | Inst In1 -> 1
  | Inst In2 -> 1
  | Inst Ini1 -> 1
  | Inst Ini2 -> 1
  | Inst Fin1 -> 0
  | Inst Fin2 -> 0
  | Inst Out1 -> 0
  | Inst Out2 -> 0
  | Inst Dup -> 2
  | Inst Xor -> 1
  | Inst Tbc -> 1
  | StackInst Swap -> 2
  | StackInst Twoswap -> 3

let n_diff = function
  | Inst In1 -> 1
  | Inst In2 -> 1
  | Inst Ini1 -> 1
  | Inst Ini2 -> 1
  | Inst Fin1 -> -1
  | Inst Fin2 -> -1
  | Inst Out1 -> -1
  | Inst Out2 -> -1
  | Inst Dup -> 1
  | Inst Xor -> -1
  | Inst Tbc -> 0
  | StackInst Swap -> 0
  | StackInst Twoswap -> 0

let from_string s =
  match String.uppercase s with
  | "IN1" -> Ok (Inst In1)
  | "IN2" -> Ok (Inst In2)
  | "INI1" -> Ok (Inst Ini1)
  | "INI2" -> Ok (Inst Ini2)
  | "FIN1" -> Ok (Inst Fin1)
  | "FIN2" -> Ok (Inst Fin2)
  | "OUT1" -> Ok (Inst Out1)
  | "OUT2" -> Ok (Inst Out2)
  | "DUP" -> Ok (Inst Dup)
  | "XOR" -> Ok (Inst Xor)
  | "TBC" -> Ok (Inst Tbc)
  | "SWAP" -> Ok (StackInst Swap)
  | "2SWAP" -> Ok (StackInst Twoswap)
  | "" -> Or_error.errorf "no instruction given"
  | _ as s -> Or_error.errorf "unknown instruction '%s'" s

let from_string_block s =
  List.map (String.split s ~on:' ') from_string |> Or_error.combine_errors

let block_length l =
  (* we do not count stack instructions as part of the block length *)
  let f acc = function
    | Inst _ -> acc + 1
    | StackInst _ -> acc
  in
  List.fold_left l ~init:0 ~f

(* counts the number of modes of size 'size' in list 'found' *)
let count found size =
  List.count found (fun l -> block_length l = size)

let count_inst l inst =
  let f acc = function
    | Inst i -> if i = inst then acc + 1 else acc
    | StackInst _ -> acc
  in
  List.fold_left l ~init:0 ~f

let print_modes found maxsize =
  for i = 1 to maxsize do
    let l = List.filter found (fun block -> block_length block = i) in
    let l = List.map l (fun block -> string_of_op_list block) in
    let l = List.sort ~compare:String.compare l in
    if List.length l > 0 then
      begin
        Printf.printf "modes of length %d:\n" i;
        List.iter l (fun block -> Printf.printf "%s\n" block)
      end
  done

let validate block phase ~simple =
  let eq x y = (Inst x) = y in
  let one inst =
    let c = List.count block (eq inst) in
    if c = 1 then Ok ()
    else Or_error.errorf "%s: Need exactly one %s instruction (%d found)"
        (string_of_phase phase) (string_of_inst inst) c
  in
  let open Or_error.Monad_infix in
  match phase with
  | Encode | Decode ->
    let l =
      if simple then [Ini1; Fin1; In1; In2; Out1; Out2]
      else [Ini1; Ini2; Fin1; Fin2; In1; In2; Out1; Out2]
    in
    Or_error.combine_errors_unit (List.map l one) >>= fun () ->
    let f acc i = acc - n_in i + n_out i in
    if List.fold block ~init:0 ~f = 0 then Ok ()
    else Or_error.errorf "%s: Leftover items on stack" (string_of_phase phase)
  | Tag ->
    let l = if simple then [Ini1; Out1] else [Ini1; Ini2; Out1] in
    Or_error.combine_errors_unit (List.map l one) >>= fun () ->
    let none = if simple then [Ini2; Out2; Fin1; Fin2] else [Out2; Fin1; Fin2] in
    let f op = List.exists none ~f:(fun inst -> op = Inst inst) in
    if List.exists block ~f then Or_error.errorf "Tag: Invalid instruction"
    else Ok ()

let is_valid block ~simple =
  let eq x y = (Inst x) = y in
  List.count block (eq Ini1) = 1
  && List.count block (eq Ini2) = (if simple then 0 else 1)
  && List.count block (eq Fin1) = 1
  && List.count block (eq Fin2) = (if simple then 0 else 1)
  && List.count block (eq In1) = 1
  && List.count block (eq In2) = 1
  && List.count block (eq Out1) = 1
  && List.count block (eq Out2) = 1
  && List.exists block (eq Tbc)
