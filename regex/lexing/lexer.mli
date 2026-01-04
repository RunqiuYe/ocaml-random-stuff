open Regex_core

type 'token t
type 'token rule = { regex : Regex.t; action : string -> 'token }

exception Lexing_error of string

val compile : 'token rule list -> 'token t
(** compile a set of rules into a lexer *)

val next : 'token t -> Lexbuf.t -> 'token
(** read one token from the input *)
