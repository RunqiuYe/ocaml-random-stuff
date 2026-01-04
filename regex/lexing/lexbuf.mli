type t

val from_string : string -> t
val from_channel : in_channel -> t
val next_char : t -> char option
val mark_start : t -> unit
val mark_accept : t -> unit
val commit : t -> string
val rollback : t -> unit
val position : t -> int
