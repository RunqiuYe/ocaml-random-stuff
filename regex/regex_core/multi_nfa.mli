type compiled
type t

val compile : Regex.t list -> compiled
(** [compile rs] compile the list of regular expressions [rs] to an NFA *)

val start : compiled -> t
(** [start comp] convert the compiled NFA [comp] to a running NFA that is ready
    to read characters and match all given patterns *)

val step : char -> t -> t
(** [step c t] step the NFA [t] with character [c] and return an NFA [t] *)

val accept_patterns : t -> int list
(** [accept_patterns t] give index of rules accepted at current state *)

val match_patterns : t -> string -> int list

val is_dead : t -> bool
(** [is_dead t] returns true if no more patterns can be continued *)

val match_prefix : t -> string -> (int * int) option
(** [match_prefix t s] returns [None] if no prefix of [s] gives a match,
    otherwise returns [Some (i, l)] where [l] is length of the longest prefix of
    [s] that is a match and [i] is the corresponding pattern being matched *)
