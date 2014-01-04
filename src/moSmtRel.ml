open Core.Std
open MoOps
module MoInst = MoInstructions

type t = {
  mutable ctr : int;
  types : string list;
  dups : string Stack.t;
  incs : string list Stack.t;
  code : string Queue.t;
  items : (string * string list) Stack.t;
}

let list_to_stack l =
  let s = Stack.create () in
  let rec loop = function
    | [] -> ()
    | x :: xs -> Stack.push s x; loop xs in
  loop (List.rev l);
  s

let create types =
  { ctr = 0;
    types = types;
    dups = list_to_stack types;
    incs = Stack.create ();
    code = Queue.create ();
    items = Stack.create ();
  }

let ctr t =
  let c = t.ctr in
  t.ctr <- t.ctr + 1;
  string_of_int c

let create_var t fn = "var" ^ "_" ^ fn ^ "_" ^ (ctr t)

let select v ty b =
  "(assert (= (select "^v^" "^ty^") "^(string_of_bool b)^"))"

let assert_diff t x y =
  let rec loop = function
    | [] -> []
    | ty :: rest ->
      ("(assert (or (= (select "^x^" "^ty^") false)
                    (= (select "^y^" "^ty^") false)))")
      :: (loop rest) in
  String.concat ~sep:"\n" (loop t.types)

let set_types t v types =
  let rec loop v = function
    | [] -> []
    | ty :: rest ->
      if List.exists types ~f:(fun t -> t = ty) then
        (select v ty true) :: (loop v rest)
      else
        (select v ty false) :: (loop v rest) in
  let l = loop v t.types in
  String.concat ~sep:"\n" l

let dup t =
  let l = create_var t "dup" in
  let r = create_var t "dup" in
  Queue.enqueue t.code ("\
;; DUP
(declare-const "^l^" (Set T))
(declare-const "^r^" (Set T))");
  let ty = Stack.pop_exn t.dups in
  let x, types = Stack.pop_exn t.items in
  let types = ty :: types in
  Queue.enqueue t.code (set_types t l types);
  Queue.enqueue t.code (set_types t r types);
  Stack.push t.items (r, types);
  Stack.push t.items (l, types)

let inc t =
  let x, types = Stack.top_exn t.items in
  (* check against all values in incs stack to make sure we haven't INCed this
     value yet *)
  let rec loop = function
    | [] -> ()
    | types' :: rest ->
      let dummy = create_var t "dummy" in
      Queue.enqueue t.code ("\
;; DUP dummy
(declare-const "^dummy^" (Set T))");
      Queue.enqueue t.code (set_types t dummy types');
      Queue.enqueue t.code (assert_diff t x dummy) in
  loop (Stack.to_list t.incs);
  (* add values types to inc stack *)
  Stack.push t.incs types

let new_item t =
  let x = create_var t "" in
  Queue.enqueue t.code ("(declare-const "^x^" (Set T))");
  Queue.enqueue t.code (set_types t x []);
  Stack.push t.items (x, [])

let prf_or_prp t =
  let _ = Stack.pop_exn t.items in
  new_item t

let tprp t =
  let _ = Stack.pop_exn t.items in
  let _ = Stack.pop_exn t.items in
  new_item t

let xor t =
  let (l, ltypes) = Stack.pop_exn t.items in
  let (r, rtypes) = Stack.pop_exn t.items in
  let outp = create_var t "xor" in
  Queue.enqueue t.code (assert_diff t l r);
  Queue.enqueue t.code ("\
;; XOR
(declare-const "^outp^" (Set T))");
  Queue.enqueue t.code (set_types t outp (List.append ltypes rtypes));
  Stack.push t.items (outp, List.append ltypes rtypes)

let pop_item t = let _ = Stack.pop_exn t.items in ()

let rec op t = function
  | Instruction i -> begin
    match i with
      | Double -> failwith "not done yet"
      | Dup -> dup t
      | Prf | Prp -> prf_or_prp t
      | TPrp -> tprp t
      | Xor -> xor t
      | Genrand | Genzero | M -> new_item t
      | Inc -> inc t
      | Nextiv_init -> ()
      | Nextiv_block | Out -> pop_item t
  end
  | StackInstruction i -> MoInst.mod_stack i t.items
  | Subroutine (_, _, _, _) -> failwith "not done yet"

let validate_related model init block =
  let ndups = MoGraph.count_inst model Dup in
  let rec loop = function
    | 0 -> []
    | _ as c -> ("R" ^ (string_of_int c)) :: loop (c - 1) in
  let types = List.rev (loop ndups) in
  let t = create types in
  let datatypes = "(declare-datatypes () ((T "
    ^ (String.concat ~sep:" " types) ^ ")))" in
  let set = "(define-sort Set (T) (Array T Bool))" in
  Queue.enqueue t.code datatypes;
  Queue.enqueue t.code set;
  let iter t l = List.iter l ~f:(op t) in
  iter t init;
  iter t block;
  Queue.enqueue t.code "(check-sat)";
  let tmp = Filename.temp_file "z3rel" ".smt2" in
  Out_channel.write_lines tmp (Queue.to_list t.code);
  let s = MoUtils.run_proc ("z3 " ^ tmp) in
  let r = begin
    match List.hd_exn (String.split s ~on:'\n') with
      | "sat" -> true
      | "unsat" -> false
      | _ -> raise (Failure ("Fatal: unknown Z3 error: " ^ s))
  end in
  Sys.remove tmp;
  r

