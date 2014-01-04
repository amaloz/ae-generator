(** MoGraph provides a graph representation of a operation stack, as well as
    functionality for checking features of the graph
*)

type t

(** [create init block] returns a new graph built from [init] and [block] *)
val create : MoInstructions.t list -> MoInstructions.t list -> t

(** [is_nextiv_value_changed t] returns [true] if the nextiv value in the graph
    is changed before it is output *)
val is_nextiv_value_changed : t -> bool

(** [is_decryptable t] returns [true] if the graph is decryptable *)
val is_decryptable : t -> bool

(** [is_connected t] returns [true] if the graph is connected *)
val is_connected : t -> bool

(** [is_pruneable t] returns [true] if the graph is pruneable *)
val is_pruneable : t -> bool

val count_inst : t -> MoOps.instruction -> int

(** [check init block] creates a graph from [init] and [block] and does a bunch
    of checks, returning [true] if the graph is a potentially valid mode *)
val check : MoInstructions.t list -> MoInstructions.t list -> bool

(** [display_with_feh t] displays the graph using feh *)
val display_with_feh : t -> unit

val display_model_with_feh : t -> (int * string) list -> unit
