open Regex_core

module type TOKEN = sig
  type t
end

module Make (Token : TOKEN) = struct
  type rule = { regex : Regex.t; action : string -> Token.t }
  type t

  let compile = failwith ""
  let read = failwith ""
end
