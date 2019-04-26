let cryptol_header = {|
foldl : {a, b, m} (fin m) => (a -> b -> a) -> a -> [m]b -> a
foldl f acc xs = ys ! 0 where ys = [acc] # [f a x | a <- ys | x <- xs]

maximum xs = foldl max 0 xs

type n = 8

// 0 -> 0
// 1 -> 1
// 2 -> $
// 3 -> ⊥
type Typ = [2]

type Ctr = [5]

ctr : [n] -> Ctr
ctr = undefined

typ : [n] -> Typ
typ = undefined

input : [n] -> [n]
input x = x

output : [n] -> [n] -> [n]
output = undefined

E : Ctr -> [n] -> [n]
E = undefined

// CTR

ctr_init x =
  ctr (input x) == (if typ x == 2 then 2 else 1)

ctr_tbc rand r x =
  ctr (E r x) ==
    (if typ x == 1 \/ typ x == 2 \/ rand then r || ctr x else ctr x)

ctr_xor x y =
  ctr (x ^ y) == (ctr x || ctr y)

ctr_out x y =
  ctr (output x y) == (if ctr x == ctr y then 0 else 1)

// TYP

typ_tbc rand r x =
  typ (E r x) ==
    (if typ x == 1 \/ typ x == 2 \/ rand then 2
      | typ x == 0 then 0
      else 3)

typ_xor x y =
  typ (x ^ y) ==
    (if typ x == 0 /\ typ y == 0 then 0
      | typ x == 0 /\ typ y == 1 then 1
      | typ x == 1 /\ typ y == 0 then 1
      | typ x == 2 /\ ctr x != ctr y then 2
      | typ y == 2 /\ ctr x != ctr y then 2
      else 3)

type block = [2][n] -> [2][n] -> [4][n]
type tag = [2][n] -> [n]

privacy : [4][n] -> [2][n] -> Bit
privacy [s0, s1, p0, p1] [t0, t1] =
  // Map(tag, ⊥, ⊥, true) → [$]
  ((typ t0 == 3 /\ typ t1 == 3) ==> (typ t == 2))
  /\
  // Map(block, ⊥, ⊥, ⊥, ⊥, true) → [·, ·, $, $] ∧ ctr(c0) ≠ ctr(c1)
  ((typ s0 == 3 /\ typ s1 == 3 /\ typ p0 == 3 /\ typ p1 == 3)
    ==> (typ c0 == 2 /\ typ c1 == 2 /\ ctr (output c0 c1) == 1))
  where [s0', _, c0, c1] = enc [ input s0, input s1, input p0, input p1 ]
        t = tag [input t0, input t1]

authenticity : [4][n] -> [2][n] -> Bit
authenticity [s0, s1, c0, c1] [t0, t1] =
  // NOTE: this check is handled by privacy check
  // // Map(tag, ⊥, ⊥, true) →  [$]
  // ((typ t0 == 3 /\ typ t1 == 3) ==> (typ t == 2))
  // /\
  // Map(block, 0, 0, 0, 1, false) →  [$, ·, ·, ·]
  ((typ s0 == 0 /\ typ s1 == 0 /\ typ c0 == 0 /\ typ c1 == 1)
    ==> (typ s0' == 2))
  /\
  // Map(block, 0, 0, 1, 0, false) →  [$, ·, ·, ·]
  ((typ s0 == 0 /\ typ s1 == 0 /\ typ c0 == 1 /\ typ c1 == 0)
    ==> (typ s0' == 2))
  /\
  // Map(block, 0, 0, 1, 1, false) →  [$, ·, ·, ·]
  ((typ s0 == 0 /\ typ s1 == 0 /\ typ c0 == 1 /\ typ c1 == 1)
    ==> (typ s0' == 2))
  /\
  // Map(block, $, 0, 0, 0, false) →  [$, ·, ·, ·]
  ((typ s0 == 2 /\ typ s1 == 0 /\ typ c0 == 0 /\ typ c1 == 0)
    ==> (typ s0' == 2))
  /\
  // Map(block, $, 0, 0, 1, false) →  [$, ·, ·, ·]
  ((typ s0 == 2 /\ typ s1 == 0 /\ typ c0 == 0 /\ typ c1 == 1)
    ==> (typ s0' == 2))
  /\
  // Map(block, $, 0, 1, 0, false) →  [$, ·, ·, ·]
  ((typ s0 == 2 /\ typ s1 == 0 /\ typ c0 == 1 /\ typ c1 == 0)
    ==> (typ s0' == 2))
  /\
  // Map(block, $, 0, 1, 1, false) →  [$, ·, ·, ·]
  ((typ s0 == 2 /\ typ s1 == 0 /\ typ c0 == 1 /\ typ c1 == 1)
    ==> (typ s0' == 2))
  /\
  // Map(block, $, 1, 0, 0, false) →  [$, ·, ·, ·]
  ((typ s0 == 2 /\ typ s1 == 1 /\ typ c0 == 0 /\ typ c1 == 0)
    ==> (typ s0' == 2))
  /\
  // Map(block, $, 1, 0, 1, false) →  [$, ·, ·, ·]
  ((typ s0 == 2 /\ typ s1 == 1 /\ typ c0 == 0 /\ typ c1 == 1)
    ==> (typ s0' == 2))
  /\
  // Map(block, $, 1, 1, 0, false) →  [$, ·, ·, ·]
  ((typ s0 == 2 /\ typ s1 == 1 /\ typ c0 == 1 /\ typ c1 == 0)
    ==> (typ s0' == 2))
  /\
  // Map(block, $, 1, 1, 1, false) →  [$, ·, ·, ·]
  ((typ s0 == 2 /\ typ s1 == 1 /\ typ c0 == 1 /\ typ c1 == 1)
    ==> (typ s0' == 2))
  /\
  // Map(tag, 1, 0, false) →  [$]
  ((typ t0 == 1 /\ typ t1 == 0) ==> (typ t == 2))
  /\
  // Map(tag, 1, 1, false) →  [$]
  ((typ t0 == 1 /\ typ t1 == 1) ==> (typ t == 2))
  where [s0', _, p0, p1] = dec [ input s0, input s1, input c0, input c1 ]
        t = tag [input t0, input t1]
|}

let saw_file = {|
r_ctr_init <- rewrite (cryptol_ss ()) (unfold_term ["ctr_init"] {{ ctr_init }});
r_ctr_tbc_priv <- rewrite (cryptol_ss ()) (unfold_term ["ctr_tbc"] {{ ctr_tbc True }});
r_ctr_tbc_auth <- rewrite (cryptol_ss ()) (unfold_term ["ctr_tbc"] {{ ctr_tbc False }});
r_ctr_xor <- rewrite (cryptol_ss ()) (unfold_term ["ctr_xor"] {{ ctr_xor }});
r_ctr_out <- rewrite (cryptol_ss ()) (unfold_term ["ctr_out"] {{ ctr_out }});
r_typ_tbc_priv <- rewrite (cryptol_ss ()) (unfold_term ["typ_tbc"] {{ typ_tbc True }});
r_typ_tbc_auth <- rewrite (cryptol_ss ()) (unfold_term ["typ_tbc"] {{ typ_tbc False }});
r_typ_xor <- rewrite (cryptol_ss ()) (unfold_term ["typ_xor"] {{ typ_xor }});

let priv_rules = [ r_ctr_init
                 , r_ctr_tbc_priv
                 , r_ctr_xor
                 , r_ctr_out
                 , r_typ_tbc_priv
                 , r_typ_xor
                 ];

let auth_rules = [ r_ctr_init
                 , r_ctr_tbc_auth
                 , r_ctr_xor
                 , r_ctr_out
                 , r_typ_tbc_auth
                 , r_typ_xor
                 ];

print "Proving privacy.";
time (prove_print do {
  goal_eval_unint ["typ", "ctr", "input", "output", "E"];
  simplify (addsimps' priv_rules (cryptol_ss ()));
  unint_z3 ["typ"];
} {{ privacy }});

print "Proving authenticity.";
time (prove_print do {
  goal_eval_unint ["typ", "ctr", "input", "output", "E"];
  simplify (addsimps' auth_rules (cryptol_ss ()));
  unint_z3 ["typ"];
} {{ authenticity }});
|}
