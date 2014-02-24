(** MoSmt provides an interface to the SMT solver *)

type t

val create : unit -> t
val op : t -> MoOps.instruction -> unit
val finalize : t -> unit
val write_to_file : t -> string -> unit
val run : string -> bool
