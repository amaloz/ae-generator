(** MoSmt provides an interface to the SMT solver *)

(** [validate ?save ?model init block] runs the mode given by [init] and [block]
    through the SMT solver, returning [true] if the scheme is satisfiable *)
val validate : ?save:(string option) -> ?model:(MoGraph.t option) ->
  MoInstructions.t list -> MoInstructions.t list -> bool
