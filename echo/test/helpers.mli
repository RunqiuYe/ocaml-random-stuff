open! Core
open Async

val launch : port:int -> uppercase:bool -> Process.t Deferred.t
(** Launches the echo server *)

val connect : port:int -> (Reader.t * Writer.t) Deferred.t
(** Connects to the echo server, returning a reader and writer for communicating
    with the server. *)

val send_data : Reader.t -> Writer.t -> string -> unit Deferred.t
(** Sends data to the server, printing out the result *)

val cleanup : Process.t -> unit Deferred.t
(** Kills the echo server, and waits until it exits *)
