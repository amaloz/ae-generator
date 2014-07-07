(** MoGraph provides a graph representation of a operation stack, as well as
    functionality for checking features of the graph
*)

type t

(** [create init block] returns a new graph built from [init] and [block] *)
val create : MoInstructions.t list -> MoInstructions.t list -> t

val assign_families : t -> t
val check : ?save:(string option) -> ?model:(t option) -> t -> bool

val is_decryptable : t -> bool
val is_valid : t -> bool

(** [display_with_feh t] displays the graph using feh *)
val display_with_feh : t -> unit
