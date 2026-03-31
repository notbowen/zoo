type t

val init : string -> t option
val next_token : t option -> (t option * Token.t) option
