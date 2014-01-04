open Core.Std
open MoOps
module MoInst = MoInstructions

type t = {
  mutable ctr : int;
  code : string Queue.t;
  items : string Stack.t;
  nextivs : string Stack.t;
}

let datatypes = "(declare-datatypes () ((T R RO RP U S SU A)))"

let create () =
  let t = { ctr = 1;
            code = Queue.create ();
            items = Stack.create ();
            nextivs = Stack.create ();
          } in
  Queue.enqueue t.code datatypes;
  t

let ctr t =
  let c = t.ctr in
  t.ctr <- t.ctr + 1;
  string_of_int c

let create_var t fn = "var" ^ "_" ^ fn ^ "_" ^ (ctr t)

let double t =
  failwith "double not done yet!"

let dup t =
  let l = create_var t "dup" in
  let r = create_var t "dup" in
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; DUP
(declare-const "^l^" T)
(declare-const "^r^" T)
(assert
 (or
  ;; DUP-1 rule
  (and (= "^x^" R)  (= "^l^" RO) (= "^r^" RP))
  (and (= "^x^" R)  (= "^l^" RP) (= "^r^" RO))
  (and (= "^x^" R)  (= "^l^" RO) (= "^r^" U))
  (and (= "^x^" R)  (= "^l^" U)  (= "^r^" RO))
  (and (= "^x^" R)  (= "^l^" RO) (= "^r^" A))
  (and (= "^x^" R)  (= "^l^" A)  (= "^r^" RO))
  (and (= "^x^" R)  (= "^l^" RP) (= "^r^" A))
  (and (= "^x^" R)  (= "^l^" A)  (= "^r^" RP))
  ;; DUP-2 rule
  (and (= "^x^" U)  (= "^l^" S)  (= "^r^" U))
  (and (= "^x^" U)  (= "^l^" U)  (= "^r^" S))
  (and (= "^x^" U)  (= "^l^" S)  (= "^r^" A))
  (and (= "^x^" U)  (= "^l^" A)  (= "^r^" S))
  (and (= "^x^" U)  (= "^l^" A)  (= "^r^" U))
  (and (= "^x^" U)  (= "^l^" U)  (= "^r^" A))
  (and (= "^x^" U)  (= "^l^" A)  (= "^r^" A))
  (and (= "^x^" RP) (= "^l^" S)  (= "^r^" U))
  (and (= "^x^" RP) (= "^l^" U)  (= "^r^" S))
  (and (= "^x^" RP) (= "^l^" S)  (= "^r^" A))
  (and (= "^x^" RP) (= "^l^" A)  (= "^r^" S))
  (and (= "^x^" RP) (= "^l^" A)  (= "^r^" U))
  (and (= "^x^" RP) (= "^l^" U)  (= "^r^" A))
  (and (= "^x^" RP) (= "^l^" A)  (= "^r^" A))
  (and (= "^x^" R)  (= "^l^" S)  (= "^r^" U))
  (and (= "^x^" R)  (= "^l^" U)  (= "^r^" S))
  (and (= "^x^" R)  (= "^l^" S)  (= "^r^" A))
  (and (= "^x^" R)  (= "^l^" A)  (= "^r^" S))
  (and (= "^x^" R)  (= "^l^" A)  (= "^r^" U))
  (and (= "^x^" R)  (= "^l^" U)  (= "^r^" A))
  (and (= "^x^" R)  (= "^l^" A)  (= "^r^" A))
  ;; DUP-3 rule
  (and (= "^x^" A)  (= "^l^" A)  (= "^r^" A))))");
  Stack.push t.items r;
  Stack.push t.items l

let genrand t =
  let v = create_var t "genrand" in
  Queue.enqueue t.code ("\
;; GENRAND
(declare-const "^v^" T)
(assert
 ;; GENRAND rule
 (or (= "^v^" R) (= "^v^" RO) (= "^v^" RP) (= "^v^" U) (= "^v^" A)))");
  Stack.push t.items v

let genzero t =
  let v = create_var t "genzero" in
  Queue.enqueue t.code ("\
;; GENZERO
(declare-const "^v^" T)
(assert
  ;; GENZERO rule
  (= "^v^" A))");
  Stack.push t.items v

let inc t =
  let v = create_var t "inc" in
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; INC
(declare-const "^v^" T)
(assert
 ;; INC rule
 (or
  (and (= "^x^" S) (= "^v^" U))
  (and (= "^x^" S) (= "^v^" A)))");
  Stack.push t.items v

let msg t =
  let v = create_var t "m" in
  Queue.enqueue t.code ("\
;; M
(declare-const "^v^" T)
(assert
 ;; M rule
 (= "^v^" A))");
  Stack.push t.items v

let nextiv t phase =
  let x = Stack.pop_exn t.items in
  match phase with
    | Init ->
      let v = create_var t "nextiv" in
      Queue.enqueue t.code ("\
;; NEXTIV
(declare-const "^v^" T)
(assert
 ;; NEXTIV rule
 (= "^v^" "^x^"))");
      Stack.push t.nextivs v;
      Stack.push t.items v
    | Block ->
      let v = Stack.pop_exn t.nextivs in
      Queue.enqueue t.code ("\
;; NEXTIV
(assert
 ;; NEXTIV rule
 (= "^x^" "^v^"))")

let out t =
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; OUT
(assert
 ;; OUT rule
 (or (= "^x^" RO) (= "^x^" R)))")

let prf_or_prp t =
  let v = create_var t "prf_or_prp" in
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; PRF/PRP
(declare-const "^v^" T)
(assert
 (or
  ;; PRF-1/PRP-1 rule
  (and (= "^x^" U)  (= "^v^" R))
  (and (= "^x^" U)  (= "^v^" RO))
  (and (= "^x^" U)  (= "^v^" RP))
  (and (= "^x^" U)  (= "^v^" U))
  (and (= "^x^" U)  (= "^v^" A))
  (and (= "^x^" RP) (= "^v^" R))
  (and (= "^x^" RP) (= "^v^" RO))
  (and (= "^x^" RP) (= "^v^" RP))
  (and (= "^x^" RP) (= "^v^" U))
  (and (= "^x^" RP) (= "^v^" A))
  (and (= "^x^" R)  (= "^v^" R))
  (and (= "^x^" R)  (= "^v^" RO))
  (and (= "^x^" R)  (= "^v^" RP))
  (and (= "^x^" R)  (= "^v^" U))
  (and (= "^x^" R)  (= "^v^" A))
  ;; PRF-2/PRP-2 rule
  (and (= "^x^" A)  (= "^v^" SU))
  (and (= "^x^" A)  (= "^v^" A))))");
  Stack.push t.items v

let tprp t =
  let v = create_var t "tprp" in
  let x = Stack.pop_exn t.items in
  let y = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; TPRP
(declare-const "^v^" T)
(assert
 (or
  ;; TPRP rule
  (and (= "^x^" U)  (= "^v^" R))
  (and (= "^y^" U)  (= "^v^" R))
  (and (= "^x^" RP) (= "^v^" R))
  (and (= "^y^" RP) (= "^v^" R))
  (and (= "^x^" R)  (= "^v^" R))
  (and (= "^y^" R)  (= "^v^" R))
  (and (= "^x^" U)  (= "^v^" RO))
  (and (= "^y^" U)  (= "^v^" RO))
  (and (= "^x^" RP) (= "^v^" RO))
  (and (= "^y^" RP) (= "^v^" RO))
  (and (= "^x^" R)  (= "^v^" RO))
  (and (= "^y^" R)  (= "^v^" RO))
  (and (= "^x^" U)  (= "^v^" RP))
  (and (= "^y^" U)  (= "^v^" RP))
  (and (= "^x^" RP) (= "^v^" RP))
  (and (= "^y^" RP) (= "^v^" RP))
  (and (= "^x^" R)  (= "^v^" RP))
  (and (= "^y^" R)  (= "^v^" RP))
  (and (= "^x^" U)  (= "^v^" U))
  (and (= "^y^" U)  (= "^v^" U))
  (and (= "^x^" RP) (= "^v^" U))
  (and (= "^y^" RP) (= "^v^" U))
  (and (= "^x^" R)  (= "^v^" U))
  (and (= "^y^" R)  (= "^v^" U))
  (and (= "^x^" U)  (= "^v^" A))
  (and (= "^y^" U)  (= "^v^" A))
  (and (= "^x^" RP) (= "^v^" A))
  (and (= "^y^" RP) (= "^v^" A))
  (and (= "^x^" R)  (= "^v^" A))
  (and (= "^y^" R)  (= "^v^" A))))");
  Stack.push t.items v

let xor t =
  let v = create_var t "xor" in
  let x = Stack.pop_exn t.items in
  let y = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; XOR
(declare-const "^v^" T)
(assert
 (or
  ;; XOR-1 rule
  (and (= "^x^" RO) (= "^v^" RO))
  (and (= "^y^" RO) (= "^v^" RO))
  (and (= "^x^" R)  (= "^v^" RO))
  (and (= "^y^" R)  (= "^v^" RO))
  (and (= "^x^" RO) (= "^v^" A))
  (and (= "^y^" RO) (= "^v^" A))
  (and (= "^x^" R)  (= "^v^" A))
  (and (= "^y^" R)  (= "^v^" A))
  ;; XOR-2 rule
  (and (= "^x^" RP) (= "^v^" RP))
  (and (= "^y^" RP) (= "^v^" RP))
  (and (= "^x^" R)  (= "^v^" RP))
  (and (= "^y^" R)  (= "^v^" RP))
  (and (= "^x^" RP) (= "^v^" A))
  (and (= "^y^" RP) (= "^v^" A))
  (and (= "^x^" R)  (= "^v^" A))
  (and (= "^y^" R)  (= "^v^" A))
  ;; XOR-3 rule
  (and (= "^x^" SU) (= "^y^" U)  (= "^v^" U))
  (and (= "^x^" U)  (= "^y^" SU) (= "^v^" U))
  (and (= "^x^" SU) (= "^y^" RP) (= "^v^" U))
  (and (= "^x^" RP) (= "^y^" SU) (= "^v^" U))
  (and (= "^x^" SU) (= "^y^" R)  (= "^v^" U))
  (and (= "^x^" R)  (= "^y^" SU) (= "^v^" U))
  (and (= "^x^" SU) (= "^y^" U)  (= "^v^" A))
  (and (= "^x^" U)  (= "^y^" SU) (= "^v^" A))
  (and (= "^x^" SU) (= "^y^" RP) (= "^v^" A))
  (and (= "^x^" RP) (= "^y^" SU) (= "^v^" A))
  (and (= "^x^" SU) (= "^y^" R)  (= "^v^" A))
  (and (= "^x^" R)  (= "^y^" SU) (= "^v^" A))
  ;; XOR-4 rule
  (= "^v^" A)))");
  Stack.push t.items v

let rec op t = function
  | Instruction i -> begin
    match i with
      | Double -> double t
      | Dup -> dup t
      | Genrand -> genrand t
      | Genzero -> genzero t
      | Inc -> inc t
      | M -> msg t
      | Nextiv_init -> nextiv t Init
      | Nextiv_block -> nextiv t Block
      | Out -> out t
      | Prf | Prp -> prf_or_prp t
      | TPrp -> tprp t
      | Xor -> xor t
  end
  | StackInstruction i ->
    MoInst.mod_stack i t.items
  | Subroutine (_, block, _, _) ->
    List.iter block (op t)

let check_sat_cmd t = Queue.enqueue t.code "(check-sat)"
let get_model_cmd t = Queue.enqueue t.code "(get-model)"

let display_model g s =
  (* XXX: complete hack! *)
  let s = String.concat ~sep:" " (String.split s ~on:'\n') in
  let extract_int s =
    let n = String.length s in
    let r = Str.regexp "[0-9]+" in
    let a = Str.search_forward r s 0 in
    let b = Str.search_backward r s n in
    int_of_string (String.slice s a (b + 1))
  in
  let rec loop s l =
    match String.prefix s 10 with
      | "define-fun" ->
        let fst = String.index_exn s ')' in
        let snd = String.index_from_exn s (fst + 1) ')' in
        let s' = String.slice s 0 snd in
        let space = String.rindex_exn s' ' ' in
        let tag = String.slice s' (space + 1) snd in
        let i = extract_int s' in
        loop (String.slice s (snd + 1) 0) ((i, tag) :: l)
      | _ -> begin
        let i = String.index s '(' in
        match i with
          | None -> l
          | Some i -> loop (String.slice s (i + 1) 0) l
      end
  in
  let l = loop s [] in
  let cmp (i, tag) (i', tag') = Int.compare i i' in
  let l = List.sort ~cmp:cmp l in
  MoGraph.display_model_with_feh g l

let validate ?(save=None) ?(model=None) init block =
  let t = create () in
  let iter t l = List.iter l ~f:(op t) in
  iter t init;
  iter t block;
  check_sat_cmd t;
  get_model_cmd t;
  let tmp = Filename.temp_file "z3" ".smt2" in
  Out_channel.write_lines tmp (Queue.to_list t.code);
  begin
    match save with
      | Some fn -> Out_channel.write_lines fn (Queue.to_list t.code)
      | None -> ()
  end;
  let s = MoUtils.run_proc ("z3 " ^ tmp) in
  let r = begin
    match List.hd_exn (String.split s ~on:'\n') with
      | "sat" -> begin
        (match model with
          | None -> ()
          | Some g -> display_model g s);
        true
      end
      | "unsat" -> false
      | _ -> raise (Failure ("Fatal: unknown Z3 error: " ^ s))
  end in
  Sys.remove tmp;
  r
