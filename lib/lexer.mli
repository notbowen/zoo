type t

val init : string -> t option
val next_token : t -> t option * Token.t
