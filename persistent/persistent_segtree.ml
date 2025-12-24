(* Implementation of persistent segment tree *)

module type MonoidType = sig
  type t

  val id : t
  val f : t -> t -> t
end

module type SegTree_intf = sig
  module Val : MonoidType

  type t

  val make : int -> Val.t -> t
  val init : int -> (int -> Val.t) -> t
  val length : t -> int

  val query : t -> int -> int -> Val.t
  (** [query] supports range query in the tree **)

  val update : t -> int -> (Val.t -> Val.t) -> t

  val set : t -> int -> Val.t -> t
  (** [update] and [set] support single point update in the tree **)
end

module MakeSegTree (Val : MonoidType) : SegTree_intf with type Val.t = Val.t =
struct
  module Val = Val

  type tree =
    | Leaf
    | Node of { reduced_value : Val.t; left : tree; right : tree }

  type t = { length : int; data : tree }

  let combine left right =
    match (left, right) with
    | Leaf, Node { reduced_value; _ } -> reduced_value
    | Node { reduced_value; _ }, Leaf -> reduced_value
    | Node { reduced_value = rval1; _ }, Node { reduced_value = rval2; _ } ->
        Val.f rval1 rval2
    | Leaf, Leaf -> Val.id

  let rec build l r init_f =
    if l < r then Leaf
    else if l = r then
      Node { reduced_value = init_f l; left = Leaf; right = Leaf }
    else
      let mid = l + ((r - l) / 2) in
      let left, right = (build l mid init_f, build (mid + 1) r init_f) in
      Node { reduced_value = combine left right; left; right }

  let rec query_helper tree l r ql qr =
    if ql <= l && r <= qr then
      match tree with
      | Leaf -> failwith "unreachable case"
      | Node { reduced_value; _ } -> reduced_value
    else
      let mid = l + ((r - l) / 2) in
      match tree with
      | Leaf -> failwith "unreachable case"
      | Node { left; right; _ } ->
          let lval =
            if ql <= mid then query_helper left l mid ql qr else Val.id
          in
          let rval =
            if qr > mid then query_helper right (mid + 1) r ql qr else Val.id
          in
          Val.f lval rval

  let rec update_helper tree l r update_i update_f =
    if l = r then
      match tree with
      | Leaf -> failwith "unreachable case"
      | Node { reduced_value; _ } ->
          Node
            {
              reduced_value = update_f reduced_value;
              left = Leaf;
              right = Leaf;
            }
    else
      let mid = l + ((r - l) / 2) in
      match tree with
      | Leaf -> failwith "unreachable case"
      | Node { left; right; _ } ->
          if update_i <= mid then
            let new_left = update_helper left l mid update_i update_f in
            Node
              { reduced_value = combine new_left right; left = new_left; right }
          else
            let new_right = update_helper right (mid + 1) r update_i update_f in
            Node
              {
                reduced_value = combine left new_right;
                left;
                right = new_right;
              }

  let make n x = { length = n; data = build 0 (n - 1) (fun _ -> x) }
  let init n f = { length = n; data = build 0 (n - 1) f }
  let length t = t.length
  let query t ql qr = query_helper t.data 0 (t.length - 1) ql qr

  let update t i f =
    { length = t.length; data = update_helper t.data 0 (t.length - 1) i f }

  let set t i x = update t i (fun _ -> x)
end
