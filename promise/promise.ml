(** A signature for Lwt-style promises, with better names *)
module type Promise = sig
  type 'a state = Pending | Resolved of 'a | Rejected of exn
  type 'a promise
  type 'a resolver

  val make : unit -> 'a promise * 'a resolver
  (** [make ()] is a new promise and resolver. The promise is pending. *)

  val return : 'a -> 'a promise
  (** [return x] is a new promise that is already resolved with value [x]. *)

  val state : 'a promise -> 'a state
  (** [state p] is the state of the promise *)

  val resolve : 'a resolver -> 'a -> unit
  (** [resolve r x] resolves the promise [p] associated with [r] with value [x],
      meaning that [state p] will become [Resolved x]. Requires: [p] is pending.
  *)

  val reject : 'a resolver -> exn -> unit
  (** [reject r x] rejects the promise [p] associated with [r] with exception
      [x], meaning that [state p] will become [Rejected x]. Requires: [p] is
      pending. *)

  val ( >>= ) : 'a promise -> ('a -> 'b promise) -> 'b promise
  (** [p >>= c] registers callback [c] with promise [p]. When the promise is
      resolved, the callback will be run on the promises's contents. If the
      promise is never resolved, the callback will never run. *)
end

module MyPromise : Promise = struct
  type 'a state = Pending | Resolved of 'a | Rejected of exn

  type 'a handler = 'a state -> unit
  (** RI: the input may not be [Pending] *)

  type 'a promise = {
    mutable state : 'a state;
    mutable handlers : 'a handler list;
  }
  (** RI: if [state <> Pending] then [handlers = []]. *)

  type 'a resolver = 'a promise

  let enqueue (handler : 'a handler) (promise : 'a promise) : unit =
    promise.handlers <- handler :: promise.handlers

  let write_once p s =
    if p.state = Pending then p.state <- s else invalid_arg "cannot write twice"

  let make () =
    let p = { state = Pending; handlers = [] } in
    (p, p)

  let return x = { state = Resolved x; handlers = [] }
  let state p = p.state

  (** requires: [st] may not be [Pending] *)
  let resolve_or_reject (r : 'a resolver) (st : 'a state) =
    assert (st <> Pending);
    let handlers = r.handlers in
    r.handlers <- [];
    write_once r st;
    List.iter (fun f -> f st) handlers

  let resolve r x = resolve_or_reject r (Resolved x)
  let reject r x = resolve_or_reject r (Rejected x)

  let handler (resolver : 'a resolver) : 'a handler = function
    | Pending -> failwith "handler RI violated"
    | Rejected exc -> reject resolver exc
    | Resolved x -> resolve resolver x

  let handler_of_callback (callback : 'a -> 'b promise)
      (output_resolver : 'b resolver) : 'a handler = function
    | Pending -> failwith "hanlder RI violated"
    | Rejected exc -> reject output_resolver exc
    | Resolved x -> (
        let promise = callback x in
        match promise.state with
        | Rejected exc -> reject output_resolver exc
        | Resolved y -> resolve output_resolver y
        | Pending -> enqueue (handler output_resolver) promise)

  let ( >>= ) (input_promise : 'a promise) (callback : 'a -> 'b promise) :
      'b promise =
    match input_promise.state with
    | Resolved x -> callback x
    | Rejected exc -> { state = Rejected exc; handlers = [] }
    | Pending ->
        let output_promise, output_resolver = make () in
        enqueue (handler_of_callback callback output_resolver) input_promise;
        output_promise
end
