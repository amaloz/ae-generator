open Core.Std

type t = { decode_s : string; tag_s : string }

let ccm = {
  decode_s =
    "INI2 DUP DUP FIN2 TBC IN2 XOR DUP OUT2 SWAP TBC IN1 XOR DUP OUT1 INI1 XOR TBC XOR TBC FIN1";
  tag_s =
    "INI1 TBC OUT1 INI2"
}

let copa = {
  decode_s =
    "IN1 TBC INI2 XOR DUP TBC DUP OUT1 INI1 XOR SWAP IN2 TBC XOR DUP FIN2 TBC DUP OUT2 XOR FIN1";
  tag_s =
    "INI1 TBC INI2 XOR TBC OUT1"
}

let copa2_s = {
  decode_s =
    "IN1 TBC INI1 XOR DUP TBC OUT1 IN2 TBC XOR DUP TBC OUT2 FIN1";
  tag_s =
    "INI1 TBC OUT1"
}

let ocb = {
  decode_s =
    "INI1 INI2 FIN2 IN1 TBC DUP OUT1 XOR IN2 TBC DUP OUT2 XOR FIN1";
  tag_s =
    "INI1 TBC OUT1 INI2"
}

let ocb_s = {
  decode_s =
    "INI1 IN1 TBC DUP OUT1 XOR IN2 TBC DUP OUT2 XOR FIN1";
  tag_s =
    "INI1 TBC OUT1"
}

let otr = {
  decode_s =
    "IN2 DUP TBC IN1 XOR DUP DUP OUT1 INI1 XOR 2SWAP SWAP TBC XOR DUP OUT2 XOR FIN1 INI2 FIN2";
  tag_s =
    "INI1 TBC OUT1 INI2"
}

let otr2_s = {
  decode_s =
    "IN2 DUP TBC IN1 XOR DUP OUT1 TBC XOR DUP OUT2 INI1 XOR FIN1";
  tag_s =
    "INI1 TBC OUT1"
}

let otr_s = {
  decode_s =
    "IN2 DUP TBC IN1 XOR DUP DUP OUT1 INI1 XOR 2SWAP SWAP TBC XOR DUP OUT2 XOR FIN1";
  tag_s =
    "INI1 TBC OUT1"
}

let xcbc = {
  decode_s =
    "INI1 INI2 IN1 DUP IN2 DUP FIN2 TBC XOR DUP OUT2 2SWAP SWAP TBC XOR DUP OUT1 2SWAP XOR XOR FIN1";
  tag_s =
    "INI1 TBC OUT1 INI2"
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

let create decode tag = { decode_s = decode; tag_s = tag }

let decode_string t = t.decode_s
let tag_string t = t.tag_s

let to_string t = t.decode_s ^ " " ^ t.tag_s
