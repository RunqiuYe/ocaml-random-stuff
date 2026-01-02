type t

val of_regex : Regex.t -> t
(** [build regex] build an NFA from a regular expression [regex] *)

val step : char -> t -> t
(** [step c t] step the NFA [t] with character [c] and return an NFA [t] *)

val is_accept : t -> bool
(** [is_accept t] returns true if the NFA [t] is currently at accept state *)

val is_match : Regex.t -> string -> bool
