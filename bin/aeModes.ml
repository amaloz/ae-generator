open AeInclude

type t = { block : op list; tag : op list }

let ccm = {
  (* "INI2 DUP DUP FIN2 TBC IN2 XOR DUP OUT2 SWAP TBC IN1 XOR DUP OUT1 INI1 XOR TBC XOR TBC FIN1" *)
  block = [Inst Ini2; Inst Dup; Inst Dup; Inst Fin2; Inst Tbc; Inst In2;
           Inst Xor; Inst Dup; Inst Out2; StackInst Swap; Inst Tbc; Inst In1;
           Inst Xor; Inst Dup; Inst Out1; Inst Ini1; Inst Xor; Inst Tbc;
           Inst Xor; Inst Tbc; Inst Fin1];
  (* "INI1 TBC OUT1 INI2" *)
  tag = [Inst Ini1; Inst Tbc; Inst Out1; Inst Ini2];
}

let copa = {
  (* "IN1 TBC INI2 XOR DUP TBC DUP OUT1 INI1 XOR SWAP IN2 TBC XOR DUP FIN2 TBC DUP OUT2 XOR FIN1" *)
  block = [Inst In1; Inst Tbc; Inst Ini2; Inst Xor; Inst Dup; Inst Tbc; Inst Dup;
           Inst Out1; Inst Ini1; Inst Xor; StackInst Swap; Inst In2; Inst Tbc;
           Inst Xor; Inst Dup; Inst Fin2; Inst Tbc; Inst Dup; Inst Out2;
           Inst Xor; Inst Fin1];
  (* "INI1 TBC INI2 XOR TBC OUT1" *)
  tag = [Inst Ini1; Inst Tbc; Inst Ini2; Inst Xor; Inst Tbc; Inst Out1];
}

let copa2_s = {
  (* "IN1 TBC INI1 XOR DUP TBC OUT1 IN2 TBC XOR DUP TBC OUT2 FIN1" *)  
  block = [Inst In1; Inst Tbc; Inst Ini1; Inst Xor; Inst Dup; Inst Tbc;
           Inst Out1; Inst In2; Inst Tbc; Inst Xor; Inst Dup; Inst Tbc;
           Inst Out2; Inst Fin1];
  (* "INI1 TBC OUT1" *)
  tag = [Inst Ini1; Inst Tbc; Inst Out1];
}

let ocb = {
  (* "INI1 INI2 FIN2 IN1 TBC DUP OUT1 XOR IN2 TBC DUP OUT2 XOR FIN1" *)
  block = [Inst Ini1; Inst Ini2; Inst Fin2; Inst In1; Inst Tbc; Inst Dup;
           Inst Out1; Inst Xor; Inst In2; Inst Tbc; Inst Dup; Inst Out2;
           Inst Xor; Inst Fin1];
  (* "INI1 TBC OUT1 INI2" *)
  tag = [Inst Ini1; Inst Tbc; Inst Out1; Inst Ini2];
}

let ocb_s = {
  (* "INI1 INI2 FIN2 IN1 TBC DUP OUT1 XOR IN2 TBC DUP OUT2 XOR FIN1" *)
  block = [Inst Ini1;Inst In1; Inst Tbc; Inst Dup; Inst Out1; Inst Xor; Inst In2;
           Inst Tbc; Inst Dup; Inst Out2; Inst Xor; Inst Fin1];
  (* "INI1 TBC OUT1" *)
  tag = [Inst Ini1; Inst Tbc; Inst Out1];
}

let otr = {
  (* "IN2 DUP TBC IN1 XOR DUP DUP OUT1 INI1 XOR 2SWAP SWAP TBC XOR DUP OUT2 XOR FIN1 INI2 FIN2" *)
  block = [Inst In2; Inst Dup; Inst Tbc; Inst In1; Inst Xor; Inst Dup; Inst Dup;
           Inst Out1; Inst Ini1; Inst Xor; StackInst Twoswap; StackInst Swap;
           Inst Tbc; Inst Xor; Inst Dup; Inst Out2; Inst Xor; Inst Fin1;
           Inst Ini2; Inst Fin2];
  (* "INI1 TBC OUT1 INI2" *)
  tag = [Inst Ini1; Inst Tbc; Inst Out1; Inst Ini2];

}

let otr2_s = {
  (* "IN2 DUP TBC IN1 XOR DUP OUT1 TBC XOR DUP OUT2 INI1 XOR FIN1" *)
  block = [Inst In2; Inst Dup; Inst Tbc; Inst In1; Inst Xor; Inst Dup; Inst Out1;
           Inst Tbc; Inst Xor; Inst Dup; Inst Out2; Inst Ini1; Inst Xor;
           Inst Fin1];
  (* "INI1 TBC OUT1" *)
  tag = [Inst Ini1; Inst Tbc; Inst Out1];
}

let otr_s = {
  (* "IN2 DUP TBC IN1 XOR DUP DUP OUT1 INI1 XOR 2SWAP SWAP TBC XOR DUP OUT2 XOR FIN1" *)
  block = [Inst In2; Inst Dup; Inst Tbc; Inst In1; Inst Xor; Inst Dup; Inst Dup;
           Inst Out1; Inst Ini1; Inst Xor; StackInst Twoswap; StackInst Swap;
           Inst Tbc; Inst Xor; Inst Dup; Inst Out2; Inst Xor; Inst Fin1];
  (* "INI1 TBC OUT1" *)
  tag = [Inst Ini1; Inst Tbc; Inst Out1];
}

let xcbc = {
  (* "INI1 INI2 IN1 DUP IN2 DUP FIN2 TBC XOR DUP OUT2 2SWAP SWAP TBC XOR DUP OUT1 2SWAP XOR XOR FIN1" *)
  block = [Inst Ini1; Inst Ini2; Inst In1; Inst Dup; Inst In2; Inst Dup;
           Inst Fin2; Inst Tbc; Inst Xor; Inst Dup; Inst Out2; StackInst Twoswap;
           StackInst Swap; Inst Tbc; Inst Xor; Inst Dup; Inst Out1;
           StackInst Twoswap; Inst Xor; Inst Xor; Inst Fin1];
  (* "INI1 TBC OUT1 INI2" *)
  tag = [Inst Ini1; Inst Tbc; Inst Out1; Inst Ini2];
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
