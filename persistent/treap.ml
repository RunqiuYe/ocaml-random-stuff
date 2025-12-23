module type ComparableType = sig
  type t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
end

module type Treap_intf = sig
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
  (** [join ltree rtree] requires all keys in ltree is less than all keys in
      rtree **)

  val join_mid : 'a t -> Key.t * 'a -> 'a t -> 'a t
  (** [join ltree m rtree] requires all keys in ltree is less than key of m, and
      key of m is less than all keys in rtree **)

  val split : Key.t -> 'a t -> 'a t * 'a option * 'a t
end

module MakeTreap (Key : ComparableType) : Treap_intf = struct
  module Key = Key

  type 'a data = { key : Key.t; value : 'a; priority : int }

  type 'a t =
    | Leaf_
    | Node_ of { data : 'a data; left : 'a t; right : 'a t; size : int }

  type 'a view =
    | Leaf
    | Node of { key : Key.t; value : 'a; left : 'a t; right : 'a t }

  let expose t =
    match t with
    | Leaf_ -> Leaf
    | Node_ { data; left; right; _ } ->
        Node { key = data.key; value = data.value; left; right }

  let size t = match t with Leaf_ -> 0 | Node_ { size; _ } -> size
  let empty () = Leaf_

  let singleton (key, value) =
    Node_
      {
        data = { key; value; priority = Key.hash key };
        size = 1;
        left = Leaf_;
        right = Leaf_;
      }

  let make_node (ltree, (key, value), rtree) =
    Node_
      {
        data = { key; value; priority = Key.hash key };
        size = size ltree + size rtree + 1;
        left = ltree;
        right = rtree;
      }

  let rec join' (ltree, rtree) =
    match (ltree, rtree) with
    | Leaf_, _ -> rtree
    | _, Leaf_ -> ltree
    | ( Node_ { data = data1; left = l1; right = r1; _ },
        Node_ { data = data2; left = l2; right = r2; _ } ) ->
        if data1.priority > data2.priority then
          make_node (l1, (data1.key, data1.value), join' (r1, rtree))
        else make_node (join' (ltree, l2), (data2.key, data2.value), r2)

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
        if Key.equal key data.key then (left, Some data.value, right)
        else if Key.compare key data.key < 0 then
          let l, res, r = split key left in
          (l, res, join r right)
        else
          let l, res, r = split key right in
          (join left l, res, r)
end
