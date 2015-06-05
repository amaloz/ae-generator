open AeOps
open Core.Std

type t = { block : op list; tag : op list }

(* XXX: FIX THESE AS THEY ARE NOW ENCODE RATHER THAN DECODE *)

let ccm = {
  block = [];
    (* "INI1 DUP DUP FIN1 TBC IN1 DUP XOR OUT1 INI2 XOR TBC SWAP IN2 DUP 2SWAP TBC XOR OUT2 XOR FIN2"; *)
  (* "INI2 DUP DUP FIN2 TBC IN2 XOR DUP OUT2 SWAP TBC IN1 XOR DUP OUT1 INI1 XOR TBC XOR TBC FIN1"; *)
  tag =
    [Inst Ini1; Inst Tbc; Inst Ini2; Inst Xor; Inst Out1];
    (* "INI1 TBC OUT1 INI2" *)
}

let copa = {
  block = [];
  (* "IN1 TBC INI2 XOR DUP TBC DUP OUT1 INI1 XOR SWAP IN2 TBC XOR DUP FIN2 TBC DUP OUT2 XOR FIN1"; *)
  tag = [];
    (* "INI1 TBC INI2 XOR TBC OUT1" *)
}

let copa2_s = {
  block = [];
    (* "IN1 TBC INI1 XOR DUP TBC OUT1 IN2 TBC XOR DUP TBC OUT2 FIN1"; *)
  tag = [];
    (* "INI1 TBC OUT1" *)
}

let ocb = {
  block = [];
    (* "INI1 IN1 DUP TBC OUT1 XOR IN2 DUP TBC OUT2 XOR FIN1 INI2 FIN2"; *)
  tag = [];
    (* "INI1 TBC OUT1 INI2" *)
}

let ocb_s = {
  block = [];
    (* "INI1 IN1 DUP TBC OUT1 XOR IN2 DUP TBC OUT2 XOR FIN1"; *)
  tag = [];
    (* "INI1 TBC OUT1" *)
}

let otr = {
  block = [];
    (* "IN2 DUP TBC IN1 XOR DUP DUP OUT1 INI1 XOR 2SWAP SWAP TBC XOR DUP OUT2 XOR FIN1 INI2 FIN2"; *)
  tag = [];
    (* "INI1 TBC OUT1 INI2" *)
}

let otr2_s = {
  block = [];
    (* "IN2 DUP TBC IN1 XOR DUP OUT1 TBC XOR DUP OUT2 INI1 XOR FIN1"; *)
  tag = [];
    (* "INI1 TBC OUT1" *)
}

let otr_s = {
  block = [];
    (* "IN2 DUP TBC IN1 XOR DUP DUP OUT1 INI1 XOR 2SWAP SWAP TBC XOR DUP OUT2 XOR FIN1"; *)
  tag = [];
    (* "INI1 TBC OUT1" *)
}

let xcbc = {
  block = [];
    (* "INI1 IN1 DUP XOR IN2 DUP XOR FIN1 SWAP INI2 XOR TBC DUP OUT1 XOR TBC DUP OUT2 FIN2"; *)
    (* "INI1 INI2 IN1 DUP IN2 DUP FIN2 TBC XOR DUP OUT2 2SWAP SWAP TBC XOR DUP OUT1 2SWAP XOR XOR FIN1"; *)
  tag = [];
    (* "INI1 TBC OUT1 INI2" *)
}

let modes =
  String.Map.of_alist_exn [
    "CCM", ccm;
    "COPA", copa;
    "COPA2-S", copa2_s;
    "OCB", ocb;
    "OTR", otr;
    "OTR2-S", otr2_s;
    "XCBC", xcbc;
    "OCB-S", ocb_s;
    "OTR-S", otr_s;
  ]

let modes_string = String.Map.keys modes |> String.concat ~sep:", "

let create block tag = { block; tag }

let block t = t.block
let tag t = t.tag

let to_string t = "[" ^ string_of_op_list t.block ^ "] [" ^ string_of_op_list t.tag ^ "]"
