type t

val build : Regex.t -> t
(** [build regex] build an NFA from a regular expression [regex].

    Requires: [regex] is in standard form. That is, for any subexpression
    [Star r], the language of [r] doe not contain the empty string. *)

val step : char -> t -> t
(** [step c t] step the NFA [t] with character [c] and return an NFA [t] *)

val is_accept : t -> bool
(** [is_accept t] returns true if the NFA [t] is currently at accept state *)
