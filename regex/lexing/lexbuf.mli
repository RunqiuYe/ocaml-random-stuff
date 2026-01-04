type t

val from_string : string -> t (* build lexbuf from string *)
val from_channel : in_channel -> t (* build lexbuf from in_channel *)
val next_char : t -> char option (* read next character *)
val mark_start : t -> unit (* mark start of token *)
val mark_accept : t -> unit (* mark accept position *)
val commit : t -> string (* commit matched token with lexeme *)
val rollback : t -> unit (* rollback to last matched position *)
val position : t -> int
