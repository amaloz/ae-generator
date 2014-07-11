open Core.Std
open MoOps

let is_valid block =
  let eq x y = (Instruction x) = y in
  List.count block (eq Out) = 1
  && List.count block (eq M) = 1
  && List.count block (eq Start) = 1
  && List.count block (eq Nextiv_block) = 1
  && List.count block (eq Genrand) = 0
  && List.exists block (eq Xor)
  && List.exists block (eq Dup)
  && (List.exists block (eq Prf) || List.exists block (eq Prp))

let is_pruneable i block =
  let cmp_prev i prev =
    let cmpi i = prev = Instruction i in
    let cmpsi i = prev = StackInstruction i in
    match i with
    | Instruction i ->
       begin
         match i with
         | Inc -> cmpi Inc || cmpi M || cmpi Prp || cmpi Prf
         | Out -> cmpi M || cmpi Inc
         | Nextiv_block -> cmpi M
         | Prf -> cmpi Prf
         | Prp -> cmpi Prp
         | Xor -> cmpi Dup
         | Dup | Genrand | M | Nextiv_init | Start -> false
       end
    | StackInstruction i ->
       begin
         match i with
         | Swap -> cmpi Dup || cmpsi Swap
         | Twoswap -> cmpsi Twoswap
       end
  in
  match block with
  | hd :: _ -> cmp_prev i hd
  | [] -> false

let msgs = ref []
let rnd = ref ""
let key = ref ""
let cipher = ref (new Cryptokit.Block.aes_encrypt "00000000000000000000000000000000")

let eval init block =
  let ofhexstr s = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) s in
  let tohexstr s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s
                   |> String.uppercase in
  let chr = function
    | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5'
    | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9' | 10 -> 'A' | 11 -> 'B'
    | 12 -> 'C' | 13 -> 'D' | 14 -> 'E' | 15 -> 'F'
    | _ -> failwith "Fatal: invalid character"
  in
  let ord = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
    | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | 'A' -> 10 | 'B' -> 11
    | 'C' -> 12 | 'D' -> 13 | 'E' -> 14 | 'F' -> 15
    | _ -> failwith "Fatal: invalid character"
  in
  let xor s s' =
    let xor c c' = chr ((ord c) lxor (ord c')) in
    let r = String.create 32 in
    for i = 0 to 32 - 1 do
      r.[i] <- xor s.[i] s'.[i]
    done;
    r
  in
  let init_random_strings () =
    Random.self_init ();
    let create () =
      let s = String.create 32 in
      for i = 0 to 32 - 1 do
        s.[i] <- Random.int 16 |> chr
      done;
      s
    in
    let rec msglist n acc =
      match n with
      | 0 -> acc
      | n when n > 0 -> msglist (n - 1) ((create ()) :: acc)
      | _ -> raise (Invalid_argument "negative values not allowed")
    in
    msgs := msglist 20 [];
    rnd := create ();
    key := create ();
    cipher := new Cryptokit.Block.aes_encrypt (ofhexstr !key);
  in
  let out = ref "" in
  let nextiv = ref "" in
  if !rnd = "" then
    init_random_strings ();
  let h = Cryptokit.Hash.md5 () in
  let s = Stack.create () in
  let visit msg inst =
    match inst with
    | Instruction i ->
       begin
         match i with
         | Dup ->
            String.copy (Stack.top_exn s) |> Stack.push s
         | Genrand ->
            Stack.push s !rnd
         | Inc ->
            let str = Stack.pop_exn s in
            let last = String.sub str 24 8 in
            let inc = int_of_string ("0x" ^ last) |> succ in
            let last = Printf.sprintf "%08X" inc in
            let newstr = (String.sub str 0 24) ^ last in
            Stack.push s newstr
         | M ->
            Stack.push s msg
         | Nextiv_init
         | Nextiv_block ->
            nextiv := Stack.pop_exn s
         | Out ->
            out := Stack.pop_exn s
         | Prf ->
            Cryptokit.hash_string h (Stack.pop_exn s |> ofhexstr)
            |> tohexstr |> Stack.push s
         | Prp ->
            let r = String.create 16 in
            !cipher#transform (Stack.pop_exn s |> ofhexstr) 0 r 0;
            tohexstr r |> Stack.push s
         | Start ->
            ()
         | Xor ->
            xor (Stack.pop_exn s) (Stack.pop_exn s) |> Stack.push s
       end
    | StackInstruction i ->
       begin
         match i with
         | Swap ->
            let first = Stack.pop_exn s in
            let second = Stack.pop_exn s in
            Stack.push s first;
            Stack.push s second
         | Twoswap ->
            let first = Stack.pop_exn s in
            let second = Stack.pop_exn s in
            let third = Stack.pop_exn s in
            Stack.push s first;
            Stack.push s second;
            Stack.push s third
       end
  in
  List.iter init (visit "");
  let rec run strings acc =
    match strings with
    | x :: xs ->
       Stack.push s !nextiv;
       List.iter block (visit x);
       run xs (!out :: !nextiv :: acc)
    | [] ->
       acc
  in
  let l = run !msgs (!out :: [!nextiv]) in
  String.concat ~sep:" " l
