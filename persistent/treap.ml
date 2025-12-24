module type ComparableType = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module type MonoidType = sig
  type t

  val id : t
  val f : t -> t -> t
end

module type Bst_intf = sig
  module Key : ComparableType

  type 'a t

  type 'a view =
    | Leaf
    | Node of { key : Key.t; value : 'a; left : 'a t; right : 'a t }

  val expose : 'a t -> 'a view
  val size : 'a t -> int
  val empty : unit -> 'a t
  val singleton : Key.t * 'a -> 'a t

  val join : 'a t -> 'a t -> 'a t
  (** [join ltree rtree] requires that all keys in [ltree] is less than all keys
      in [rtree] **)

  val join_mid : 'a t -> Key.t * 'a -> 'a t -> 'a t
  (** [join ltree m rtree] requires that all keys in [ltree] is less than key of
      [m], and key of [m] is less than all keys in [rtree] **)

  val split : Key.t -> 'a t -> 'a t * 'a option * 'a t
end

module type Aug_Bst_intf = sig
  module Key : ComparableType
  module Val : MonoidType

  type t

  type view =
    | Leaf
    | Node of { key : Key.t; value : Val.t; left : t; right : t }

  val expose : t -> view
  val size : t -> int
  val empty : unit -> t
  val singleton : Key.t * Val.t -> t
  val get_reduced_val : t -> Val.t
  val join : t -> t -> t
  val join_mid : t -> Key.t * Val.t -> t -> t
  val split : Key.t -> t -> t * Val.t option * t
end

module MakeTreapBase (Key : ComparableType) (Val : MonoidType) = struct
  module Key = Key
  module Val = Val

  type 'a data = { key : Key.t; value : 'a; rvalue : Val.t; priority : int }

  type 'a t =
    | Leaf_
    | Node_ of {
        data : 'a data;
        reduced_value : Val.t;
        left : 'a t;
        right : 'a t;
        size : int;
      }

  type 'a view =
    | Leaf
    | Node of {
        key : Key.t;
        value : 'a;
        rvalue : Val.t;
        left : 'a t;
        right : 'a t;
      }

  let expose t =
    match t with
    | Leaf_ -> Leaf
    | Node_ { data; left; right; _ } ->
        Node
          {
            key = data.key;
            value = data.value;
            rvalue = data.rvalue;
            left;
            right;
          }

  let size t = match t with Leaf_ -> 0 | Node_ { size; _ } -> size
  let empty () = Leaf_

  let get_reduced_val tree =
    match tree with
    | Leaf_ -> Val.id
    | Node_ { reduced_value; _ } -> reduced_value

  let make_node (ltree, data, rtree) =
    Node_
      {
        data;
        size = size ltree + size rtree + 1;
        left = ltree;
        right = rtree;
        reduced_value =
          Val.f (get_reduced_val ltree)
            (Val.f data.rvalue (get_reduced_val rtree));
      }

  let singleton (key, value, rvalue) =
    make_node (Leaf_, { key; value; rvalue; priority = Key.hash key }, Leaf_)

  let rec join' (ltree, rtree) =
    match (ltree, rtree) with
    | Leaf_, _ -> rtree
    | _, Leaf_ -> ltree
    | ( Node_ { data = data1; left = l1; right = r1; _ },
        Node_ { data = data2; left = l2; right = r2; _ } ) ->
        if data1.priority > data2.priority then
          make_node (l1, data1, join' (r1, rtree))
        else make_node (join' (ltree, l2), data2, r2)

  let rec first tree =
    match tree with
    | Leaf_ -> None
    | Node_ { data; left; _ } -> (
        match left with Leaf_ -> Some data.key | _ -> first left)

  let rec last tree =
    match tree with
    | Leaf_ -> None
    | Node_ { data; right; _ } -> (
        match right with Leaf_ -> Some data.key | _ -> last right)

  let join ltree rtree =
    match (last ltree, first rtree) with
    | Some left_last, Some right_first ->
        if Key.compare left_last right_first >= 0 then
          raise
            (Invalid_argument
               "keys in ltree is not strictly smaller than keys in rtree")
        else join' (ltree, rtree)
    | _ -> join' (ltree, rtree)

  let join_mid ltree m rtree = join ltree (join (singleton m) rtree)

  let rec split key t =
    match t with
    | Leaf_ -> (Leaf_, None, Leaf_)
    | Node_ { data; left; right; _ } ->
        if Key.equal key data.key then
          (left, Some (data.value, data.rvalue), right)
        else if Key.compare key data.key < 0 then
          let l, res, r = split key left in
          (l, res, join r right)
        else
          let l, res, r = split key right in
          (join left l, res, r)
end

module MakeTreap (Key : ComparableType) : Bst_intf with type Key.t = Key.t =
struct
  module TrivialMonoid : MonoidType = struct
    type t = unit

    let id = ()
    let f () () = ()
  end

  module TreapBase = MakeTreapBase (Key) (TrivialMonoid)
  module Key = Key

  type 'a t = 'a TreapBase.t

  type 'a view =
    | Leaf
    | Node of { key : Key.t; value : 'a; left : 'a t; right : 'a t }

  let expose t =
    match TreapBase.expose t with
    | Leaf -> Leaf
    | Node { key; value; left; right; _ } -> Node { key; value; left; right }

  let size t = TreapBase.size t
  let empty t = TreapBase.empty t
  let singleton (key, value) = TreapBase.singleton (key, value, TrivialMonoid.id)
  let join = TreapBase.join

  let join_mid ltree (key, value) rtree =
    TreapBase.join_mid ltree (key, value, TrivialMonoid.id) rtree

  let split key t =
    let l, res, r = TreapBase.split key t in
    (l, Option.map (fun (v, _) -> v) res, r)
end

module MakeAugTreap (Key : ComparableType) (Val : MonoidType) :
  Aug_Bst_intf with type Key.t = Key.t and type Val.t = Val.t = struct
  module TreapBase = MakeTreapBase (Key) (Val)

  type t = unit TreapBase.t

  module Key = Key
  module Val = Val

  type view =
    | Leaf
    | Node of { key : Key.t; value : Val.t; left : t; right : t }

  let expose t =
    match TreapBase.expose t with
    | Leaf -> Leaf
    | Node { key; rvalue; left; right; _ } ->
        Node { key; value = rvalue; left; right }

  let size = TreapBase.size
  let empty = TreapBase.empty
  let get_reduced_val = TreapBase.get_reduced_val
  let singleton (key, value) = TreapBase.singleton (key, (), value)
  let join = TreapBase.join

  let join_mid ltree (key, value) rtree =
    TreapBase.join_mid ltree (key, (), value) rtree

  let split key t =
    let l, res, r = TreapBase.split key t in
    (l, Option.map (fun (_, rval) -> rval) res, r)
end
