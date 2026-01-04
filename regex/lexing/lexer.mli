open Regex_core

module type TOKEN = sig
  type t
end

module Make (Token : TOKEN) : sig
  type rule = { regex : Regex.t; action : string -> Token.t }
  type t

  val compile : rule list -> t
  val read : t -> Lexbuf.t -> Token.t
end
