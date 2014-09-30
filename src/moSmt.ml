open Core.Std
open MoOps
module MoInst = MoInstructions

type labelvars = {
  typ : string;
  flag_inc : string;
  flag_incd : string;
  flag_out : string;
  flag_prf : string;
}

type t = {
  mutable ctr : int;
  code : string Queue.t;
  items : labelvars Stack.t;
  mutable start_vars : labelvars;
}

let create () =
  let t = { ctr = 1;
            code = Queue.create ();
            items = Stack.create ();
            start_vars = { typ = ""; flag_inc = ""; flag_incd = "";
                           flag_out = ""; flag_prf = "" };
          } in
  let init = "\
(declare-const R Int)
(declare-const U Int)
(declare-const B Int)
(assert (= R 2))
(assert (= U 1))
(assert (= B 0))" in
  Queue.enqueue t.code init;
  t

let create_vars t fn =
  let f s = s ^ "_" ^ fn ^ "_" ^ (string_of_int t.ctr) in
  let typ = f "var" in
  let flag_inc = f "flag_inc" in
  let flag_incd = f "flag_incd" in
  let flag_out = f "flag_out" in
  let flag_prf = f "flag_prf" in
  let r = { typ = typ; flag_inc = flag_inc; flag_incd = flag_incd;
            flag_out = flag_out; flag_prf = flag_prf } in
  t.ctr <- t.ctr + 1;
  r

let declare_consts x = "\
(declare-const "^x.typ^" Int)
(declare-const "^x.flag_inc^" Bool)
(declare-const "^x.flag_incd^" Bool)
(declare-const "^x.flag_out^" Bool)
(declare-const "^x.flag_prf^" Bool)"

let dup t =
  let l = create_vars t "dup_l" in
  let r = create_vars t "dup_r" in
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; DUP
"^(declare_consts l)^"
"^(declare_consts r)^"
(assert (= "^l.typ^" "^r.typ^" "^x.typ^"))
(assert (= (and "^l.flag_inc^" "^r.flag_inc^") false))
(assert (= (and "^l.flag_out^" "^r.flag_out^") false))
(assert (= (and "^l.flag_prf^" "^r.flag_prf^") false))
(assert (= (or "^l.flag_inc^" "^r.flag_inc^") "^x.flag_inc^"))
(assert (= (or "^l.flag_out^" "^r.flag_out^") "^x.flag_out^"))
(assert (= (or "^l.flag_prf^" "^r.flag_prf^") "^x.flag_prf^"))
(assert (= "^l.flag_incd^" "^r.flag_incd^" "^x.flag_incd^"))");
  Stack.push t.items r;
  Stack.push t.items l

let genrand t =
  let v = create_vars t "genrand" in
  Queue.enqueue t.code ("\
;; GENRAND
"^(declare_consts v)^"
(assert (= "^v.typ^" R))
(assert (= "^v.flag_prf^" "^v.flag_out^" true))
(assert (= "^v.flag_inc^" "^v.flag_incd^"))");
  Stack.push t.items v

let inc t =
  let v = create_vars t "inc" in
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; INC
(assert (or (= "^x.typ^" R) (= "^x.typ^" U)))
(assert (= "^x.flag_inc^" true))
"^(declare_consts v)^"
(assert (= "^v.typ^" U))
(assert (= "^v.flag_inc^" "^v.flag_incd^" "^v.flag_prf^" true))
(assert (= "^v.flag_out^" false)) ");
  Stack.push t.items v

let msg t =
  let v = create_vars t "m" in
  Queue.enqueue t.code ("\
;; M
"^(declare_consts v)^"
(assert (= "^v.typ^" B))
(assert (= "^v.flag_inc^" "^v.flag_incd^" "^v.flag_out^" "^v.flag_prf^" false))");
  Stack.push t.items v

let lt_bool a b = "\
(assert (if (= "^b^" false)
            (= "^a^" false)
            (or (= "^a^" false) (= "^a^" true))))"


let nextiv t phase =
  let v = create_vars t "nextiv" in
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; NEXTIV
"^(declare_consts v)^"
(assert (= "^v.typ^" "^x.typ^"))
(assert (= "^v.flag_inc^" "^x.flag_inc^"))
(assert (= "^v.flag_incd^" "^x.flag_incd^"))
(assert (= "^v.flag_out^" "^x.flag_out^"))
(assert (= "^v.flag_prf^" "^x.flag_prf^"))");
  begin
    match phase with
    | Init ->
      Stack.push t.items v
    | Block ->
      begin
        Queue.enqueue t.code ("\
(assert (<= "^t.start_vars.typ^" "^v.typ^"))
(assert (= "^t.start_vars.flag_incd^" "^v.flag_incd^"))");
        Queue.enqueue t.code (lt_bool t.start_vars.flag_inc v.flag_inc);
        (* TODO: missing INCd here? *)
        Queue.enqueue t.code (lt_bool t.start_vars.flag_out v.flag_out);
        Queue.enqueue t.code (lt_bool t.start_vars.flag_prf v.flag_prf);
      end
  end

let out t =
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; OUT
(assert (= "^x.typ^" R))
(assert (= "^x.flag_out^" true))")

let prf t =
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; PRF
(assert (or (= "^x.typ^" R) (= "^x.typ^" U)))
(assert (= "^x.flag_prf^" true))");
  genrand t

let start t =
  let v = create_vars t "start" in
  let x = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; START
"^(declare_consts v)^"
(assert (<= "^v.typ^" "^x.typ^"))
(assert (= "^v.flag_incd^" "^x.flag_incd^"))");
  Queue.enqueue t.code (lt_bool v.flag_inc x.flag_inc);
  Queue.enqueue t.code (lt_bool v.flag_out x.flag_out);
  Queue.enqueue t.code (lt_bool v.flag_prf x.flag_prf);
  t.start_vars <- v;
  Stack.push t.items v

let xor t =
  let v = create_vars t "xor" in
  let x = Stack.pop_exn t.items in
  let y = Stack.pop_exn t.items in
  Queue.enqueue t.code ("\
;; XOR
"^(declare_consts v)^"
(assert (or (= "^x.typ^" R) (= "^y.typ^" R)))
(assert (= "^x.flag_incd^" "^y.flag_incd^" false))
(assert (= "^v.typ^" R))
(assert (= "^v.flag_inc^" "^v.flag_incd^" false))
(assert (if (= "^x.typ^" R)
            (if (= "^y.typ^" R)
                (and
                 (= (or "^x.flag_out^" "^y.flag_out^") "^v.flag_out^")
                 (= (or "^x.flag_prf^" "^y.flag_prf^") "^v.flag_prf^"))
                (and
                 (= "^x.flag_out^" "^v.flag_out^")
                 (= "^x.flag_prf^" "^v.flag_prf^")))
            (and
             (= "^y.flag_prf^" "^v.flag_prf^")
             (= "^y.flag_out^" "^v.flag_out^"))))");
  Stack.push t.items v

let op t = function
  | Dup -> dup t
  | Genrand -> genrand t
  | Inc -> inc t
  | M -> msg t
  | Nextiv_init -> nextiv t Init
  | Nextiv_block -> nextiv t Block
  | Out -> out t
  | Prf | Prp -> prf t
  | Start -> start t
  | Xor -> xor t

let finalize t =
  Queue.enqueue t.code "(check-sat) (get-model)"

let write_to_file t f =
  Out_channel.write_lines f (Queue.to_list t.code)

(* let display_model g s = *)
(*   (\* XXX: complete hack! *\) *)
(*   let s = String.concat ~sep:" " (String.split s ~on:'\n') in *)
(*   let extract_int s = *)
(*     let n = String.length s in *)
(*     let r = Str.regexp "[0-9]+" in *)
(*     let a = Str.search_forward r s 0 in *)
(*     let b = Str.search_backward r s n in *)
(*     int_of_string (String.slice s a (b + 1)) *)
(*   in *)
(*   let rec loop s l = *)
(*     match String.prefix s 10 with *)
(*       | "define-fun" -> *)
(*         let fst = String.index_exn s ')' in *)
(*         let snd = String.index_from_exn s (fst + 1) ')' in *)
(*         let s' = String.slice s 0 snd in *)
(*         let space = String.rindex_exn s' ' ' in *)
(*         let tag = String.slice s' (space + 1) snd in *)
(*         let i = extract_int s' in *)
(*         loop (String.slice s (snd + 1) 0) ((i, tag) :: l) *)
(*       | _ -> begin *)
(*         let i = String.index s '(' in *)
(*         match i with *)
(*           | None -> l *)
(*           | Some i -> loop (String.slice s (i + 1) 0) l *)
(*       end *)
(*   in *)
(*   let l = loop s [] in *)
(*   let cmp (i, tag) (i', tag') = Int.compare i i' in *)
(*   let l = List.sort ~cmp:cmp l in *)
(*   MoGraph.display_model_with_feh g l *)

let run fname =
  let s = MoUtils.run_proc ("z3 " ^ fname) in
  match List.hd_exn (String.split s ~on:'\n') with
  | "sat" -> true
  | "unsat" -> false
  | _ -> raise (Failure ("Fatal: unknown Z3 error: " ^ s))


