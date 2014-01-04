(** MoInstructions provides an interface to the operations of molang
*)

open Core.Std
open MoOps

type t = operations

(** [n_in t] returns the number of inputs to the given instruction *)
val n_in : t -> int

(** [n_out t] returns the number of outputs from the given instruction *)
val n_out : t -> int

(** [string_of_t t] returns the given instruction as a string *)
val string_of_t : t -> string

(** [string_of_t_list l] returns the given list of instructions as a string *)
val string_of_t_list : t list -> string

(** [from_string s phase table] returns the instruction represented by the given
    string *)
val from_string : string -> phase -> (string, t) String.Table.t_ -> t

val from_string_block : string -> phase -> (string, t) String.Table.t_ -> t list

val mod_stack : stackInstruction -> 'a Stack.t -> unit

val n_ins_and_outs : t list -> int * int

val load_subroutines : string -> (string, t) String.Table.t_
