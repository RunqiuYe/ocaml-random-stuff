(* Implementation of persistent segment tree *)

type 'a tree = 'a data ref
and 'a data = Empty | Leaf of 'a | Node of 'a * 'a tree * 'a tree

type 'a t = { length : int; pull : 'a -> 'a -> 'a; tree : 'a tree }

let combine pull ltree rtree =
  match (!ltree, !rtree) with
  | Leaf x, Leaf y -> pull x y
  | Leaf x, Node (y, _, _) -> pull x y
  | Node (x, _, _), Leaf y -> pull x y
  | Node (x, _, _), Node (y, _, _) -> pull x y
  | _ -> failwith "unreachable case"

let rec build l r pull init_f =
  if l < r then ref Empty
  else if l = r then ref (Leaf (init_f l))
  else
    let mid = l + ((r - l) / 2) in
    let ltree, rtree =
      (build l mid pull init_f, build (mid + 1) r pull init_f)
    in
    ref (Node (combine pull ltree rtree, ltree, rtree))

let make n pull x =
  { length = n; pull; tree = build 0 (n - 1) pull (fun _ -> x) }

let init n pull f = { length = n; pull; tree = build 0 (n - 1) pull f }
let length t = t.length

let rec query_helper tree pull l r ql qr =
  if ql <= l && r <= qr then
    match !tree with
    | Leaf x -> x
    | Node (x, _, _) -> x
    | _ -> failwith "unreachable case"
  else
    let mid = l + ((r - l) / 2) in
    match !tree with
    | Node (_, ltree, rtree) ->
        if ql <= mid && qr > mid then
          pull
            (query_helper ltree pull l mid ql qr)
            (query_helper rtree pull (mid + 1) r ql qr)
        else if ql <= mid then query_helper ltree pull l mid ql qr
        else if qr > mid then query_helper rtree pull (mid + 1) r ql qr
        else failwith "unreachable case"
    | _ -> failwith "unreachable case"

let query t ql qr =
  if 0 <= ql && ql <= qr && qr < t.length then
    query_helper t.tree t.pull 0 (t.length - 1) ql qr
  else raise (Invalid_argument "invalid range")

let rec update_helper tree pull l r i update_f =
  if l = r then
    match !tree with
    | Leaf x -> ref (Leaf (update_f x))
    | _ -> failwith "unreachable case"
  else
    let mid = l + ((r - l) / 2) in
    match !tree with
    | Node (_, ltree, rtree) ->
        if i <= mid then
          let new_ltree = update_helper ltree pull l mid i update_f in
          ref (Node (combine pull new_ltree rtree, new_ltree, rtree))
        else
          let new_rtree = update_helper rtree pull (mid + 1) r i update_f in
          ref (Node (combine pull ltree new_rtree, ltree, new_rtree))
    | _ -> failwith "unreachable case"

let update t i update_f =
  let n = t.length in
  if i < 0 || i >= n then raise (Invalid_argument "out of bound access")
  else
    {
      length = n;
      pull = t.pull;
      tree = update_helper t.tree t.pull 0 (n - 1) i update_f;
    }

let set t i x = update t i (fun _ -> x)
