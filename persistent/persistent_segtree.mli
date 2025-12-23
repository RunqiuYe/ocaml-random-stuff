(* Interface for persistent segment tree *)

type 'a t

val make : int -> ('a -> 'a -> 'a) -> 'a -> 'a t
val init : int -> ('a -> 'a -> 'a) -> (int -> 'a) -> 'a t
val length : 'a t -> int
val query : 'a t -> int -> int -> 'a
val update : 'a t -> int -> ('a -> 'a) -> 'a t
val set : 'a t -> int -> 'a -> 'a t
