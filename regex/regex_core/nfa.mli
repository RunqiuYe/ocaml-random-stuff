type t

val of_regex : Regex.t -> t
(** [build regex] build an NFA from a regular expression [regex] *)

val step : char -> t -> t
(** [step c t] step the NFA [t] with character [c] and return an NFA [t] *)

val is_accept : t -> bool
(** [is_accept t] returns true if the NFA [t] is currently at accept state *)

val is_dead : t -> bool
(** [is_dead t] returns true if the NFA [t] has no active state *)

val is_match : t -> string -> bool
(** [is_match t s] returns true if the NFA [t] accepts the string [s] *)

val match_prefix : t -> string -> int option
(** [match_prefix t s] returns [None] if no prefix of [s] gives a match,
    otherwise returns [Some l] where [l] is the length of the longest prefix of
    [s] that is a match *)
