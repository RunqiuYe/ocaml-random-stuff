open Regex_core

type 'token t = int
type 'token rule = { regex : Regex.t; action : string -> 'token }

exception Lexing_error of string

let compile = failwith "unimplemented"
let next = failwith "unimplemented"
