type t

val create : AeOps.operations list -> AeOps.phase -> t
val display_with_feh : t -> unit
val derive_encode_graph : t -> t
val is_secure : t -> bool
