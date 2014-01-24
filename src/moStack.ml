open Core.Std
open MoOps
module MoInst = MoInstructions

let ord = function
  | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
  | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'a' -> 10 | 'b' -> 11
  | 'c' -> 12 | 'd' -> 13 | 'e' -> 14 | 'f' -> 15
  | _ -> failwith "Fatal: invalid character"

let chr = function
  | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5'
  | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9' | 10 -> 'a' | 11 -> 'b'
  | 12 -> 'c' | 13 -> 'd' | 14 -> 'e' | 15 -> 'f'
  | _ -> failwith "Fatal: invalid character"

let hex s = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) s
let tohex s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s

let xor s s' =
  assert ((String.length s) = (String.length s'));
  let len = String.length s in
  let xor c c' = chr ((ord c) lxor (ord c')) in
  let r = String.create len in
  for i = 0 to len - 1 do
    r.[i] <- xor s.[i] s'.[i]
  done;
  assert ((String.length r) = (String.length s));
  r

(* XXX: only increments lsb! *)
let inc s =
  let inc c =
    let r = ord(c) + 1 in
    if r > 15 then chr(0) else chr(r)
  in
  let len = String.length s in
  let r = String.copy s in
  r.[len-1] <- inc s.[len-1];
  r

let print_stack s =
  let str = List.to_string ~f:(fun x -> tohex x) (Stack.to_list s) in
  Printf.printf "STACK: %s\n" str

let eval init block =
  (* XXX: this is an awful hack! *)
  let out = ref [] in
  (* let empty_str () = *)
  (*   let s = String.create 16 in *)
  (*   String.fill s ~pos:0 ~len:16 '\000'; *)
  (*   s in *)
  let msg = "12345678123456781234567812345678" in
  let key = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
  let rnd = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
  let c = new Cryptokit.Block.aes_encrypt (hex key) in
  let rec run_t s s' = function
    | Instruction i ->
       begin
         match i with
         | Dup ->
            let r = String.copy (Stack.top_exn s) in
            Stack.push s r
         | Genrand -> Stack.push s (hex rnd)
         (* | Inc -> Stack.push s (hex (inc (tohex (Stack.pop_exn s)))) *)
         | M -> Stack.push s (hex msg)
         | Nextiv_init
         | Nextiv_block
         | Start -> Stack.push s' (Stack.pop_exn s)
         | Out -> out := (tohex (Stack.pop_exn s)) :: !out
         | Prf ->
            let h = Cryptokit.Hash.md5 () in
            let r = Cryptokit.hash_string h (Stack.pop_exn s) in
            Stack.push s r
         | Prp ->
           let r = String.create 16 in
           c#transform (Stack.pop_exn s) 0 r 0;
           Stack.push s r
         | Xor ->
            let s1 = Stack.pop_exn s in
            let s2 = Stack.pop_exn s in
            let r = xor (tohex s1) (tohex s2) in
            Stack.push s (hex r)
    end
    | StackInstruction i ->
      MoInst.mod_stack i s
  in
  let rec f l s s' = match l with
    | i :: rest -> run_t s s' i; f rest s s'
    | [] -> () in
  let s = Stack.create () in
  let s' = Stack.create () in
  f init s s';
  let s'' = Stack.create () in
  f block s' s'';
  let s''' = Stack.create () in
  f block s'' s''';
  List.rev !out

let is_valid block =
  let eq x y = (Instruction x) = y in
  not (List.count block (eq Out) <> 1
       || List.count block (eq M) <> 1
       || List.count block (eq Start) <> 1
       || List.count block (eq Nextiv_block) <> 1
       || List.count block (eq Genrand) <> 0)

let is_pruneable i block =
  let cmp_prev i prev =
    let cmpi i = prev = Instruction i in
    let cmps i = prev = StackInstruction i in
    match i with
    | Instruction i ->
       begin
         match i with
         | Dup -> cmpi Dup
         (* | Inc -> cmpi M || cmpi Inc || cmpi Prp || cmpi Genrand || cmpi Genzero *)
         | Out -> cmpi M || (* cmpi Inc || *) cmpi Genrand
         | Nextiv_block -> cmpi M
         | Prf | Prp -> cmpi Prf || cmpi Prp || cmpi Genrand
         | Xor -> cmpi Dup
         | Genrand | M | Nextiv_init | Start -> false
       end
    | StackInstruction i ->
       begin
         match i with
         | Swap -> cmpi Dup || cmps Swap
         | Twoswap -> cmps Twoswap
       end
  in
  let cmp_2prev i p p' =
    let cmpi_p i = p = Instruction i in
    let cmpi_p' i = p' = Instruction i in
    match i with
    | Instruction i ->
       begin
         match i with
         | Out -> cmpi_p' M && (cmpi_p Prp || cmpi_p Dup)
         | _ -> false
       end
    | _ -> false
  in
  match block with
  | hd :: hd' :: _ -> cmp_prev i hd || cmp_2prev i hd hd'
  | hd :: _ -> cmp_prev i hd
  | [] -> false
