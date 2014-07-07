open Core.Std
open MoOps

let is_valid block =
  let eq x y = (Instruction x) = y in
  List.count block (eq Out) = 1
  && List.count block (eq M) = 1
  && List.count block (eq Start) = 1
  && List.count block (eq Nextiv_block) = 1
  && List.count block (eq Genrand) = 0
  && (List.count block (eq Prf) >= 1
      || List.count block (eq Prp) >= 1)

let is_pruneable i block =
  let cmp_prev i prev =
    let cmpi i = prev = Instruction i in
    let cmps i = prev = StackInstruction i in
    match i with
    | Instruction i ->
       begin
         match i with
         | Dup -> cmpi Dup
         | Inc -> cmpi M || cmpi Inc || cmpi Prp || cmpi Genrand
         | Out -> cmpi M || cmpi Inc || cmpi Genrand
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

let eval init block =
  let ofhexstr s = Cryptokit.transform_string (Cryptokit.Hexa.decode ()) s in
  let tohexstr s = Cryptokit.transform_string (Cryptokit.Hexa.encode ()) s in
  let chr = function
    | 0 -> '0' | 1 -> '1' | 2 -> '2' | 3 -> '3' | 4 -> '4' | 5 -> '5'
    | 6 -> '6' | 7 -> '7' | 8 -> '8' | 9 -> '9' | 10 -> 'A' | 11 -> 'B'
    | 12 -> 'C' | 13 -> 'D' | 14 -> 'E' | 15 -> 'F'
    | _ -> failwith "Fatal: invalid character"
  in
  let ord = function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
    | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9
    | 'A' -> 10 | 'B' -> 11 | 'C' -> 12 | 'D' -> 13
    | 'E' -> 14 | 'F' -> 15
    | _ -> failwith "Fatal: invalid character"
  in
  let xor s s' =
    assert (String.length s = String.length s');
    let len = String.length s in
    let xor c c' = chr ((ord c) lxor (ord c')) in
    let r = String.create len in
    for i = 0 to len - 1 do
      r.[i] <- xor s.[i] s'.[i]
    done;
    r
  in
  let out = ref "" in
  let nextiv = ref "" in
  let msg = "12345678123456781234567812345678" in
  let key = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
  let rnd = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" in
  let c = new Cryptokit.Block.aes_encrypt (ofhexstr key) in
  let h = Cryptokit.Hash.md5 () in
  let s = Stack.create () in
  let visit inst =
    match inst with
    | Instruction i ->
       begin
         match i with
         | Dup ->
            String.copy (Stack.top_exn s) |> Stack.push s
         | Genrand ->
            begin
              Stack.push s rnd
            end
         | Inc ->
            let str = Stack.pop_exn s in
            (* We need this copy, because OCaml strings should not be modified in
       place! *)
            let str = String.copy str in
            let len = String.length str in
            begin
              try
                str.[len-1] <- ord str.[len-1] |> (+) 1 |> chr
              with _ ->
                str.[len-1] <- '0'
            end;
            Stack.push s str
         | M ->
            Stack.push s msg
         | Nextiv_init
         | Start ->
            ()
         | Nextiv_block ->
            nextiv := Stack.pop_exn s
         | Out ->
            out := Stack.pop_exn s
         | Prf ->
            Cryptokit.hash_string h (Stack.pop_exn s |> ofhexstr)
            |> tohexstr
            |> String.uppercase
            |> Stack.push s
         | Prp ->
            let r = String.create 16 in
            c#transform (Stack.pop_exn s |> ofhexstr) 0 r 0;
            tohexstr r |> String.uppercase |> Stack.push s
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
  List.iter init visit;
  List.iter block visit;
  assert (Stack.length s = 0);
  (* let l = List.sort String.compare (!out :: [!nextiv]) in *)
  String.concat ~sep:" " (!out :: [!nextiv])
