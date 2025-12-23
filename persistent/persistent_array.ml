(* Implementation of persistent array using sparse segment tree *)

open Base

type 'a tree = Empty | Leaf of 'a | Node of 'a tree * 'a tree
type 'a t = { length : int; tree : 'a tree }

let rec build l r f =
  if l > r then Empty
  else if l = r then Leaf (f l)
  else
    let mid = l + ((r - l) / 2) in
    Node (build l mid f, build (mid + 1) r f)

let rec query tree l r i =
  if l = r then match tree with Leaf x -> x | _ -> failwith "unreachable case"
  else
    match tree with
    | Node (ltree, rtree) ->
        let mid = l + ((r - l) / 2) in
        if i <= mid then query ltree l mid i else query rtree (mid + 1) r i
    | _ -> failwith "unreachable case"

let rec update tree l r i x =
  if l = r then Leaf x
  else
    match tree with
    | Node (ltree, rtree) ->
        let mid = l + ((r - l) / 2) in
        if i <= mid then Node (update ltree l mid i x, rtree)
        else Node (ltree, update rtree (mid + 1) r i x)
    | _ -> failwith "unreachable case"

let make n x = { length = n; tree = build 0 (n - 1) (fun _ -> x) }
let init n f = { length = n; tree = build 0 (n - 1) f }
let length t = t.length

let get t i =
  if i < 0 || i >= t.length then raise (Invalid_argument "out of bound access")
  else query t.tree 0 (t.length - 1) i

let set t i x =
  if i < 0 || i >= t.length then raise (Invalid_argument "out of bound access")
  else { length = t.length; tree = update t.tree 0 (t.length - 1) i x }

let to_list t = List.init t.length ~f:(fun i -> get t i)

let iter f t =
  for i = 0 to t.length - 1 do
    f (get t i)
  done

let iteri f t =
  for i = 0 to t.length - 1 do
    f i (get t i)
  done

let rec fold_left_cps f acc t i =
  let n = t.length in
  if i = n - 1 then f acc (get t (n - 1))
  else fold_left_cps f (f acc (get t i)) t (i + 1)

let rec fold_right_cps f t acc i =
  if i = 0 then f (get t 0) acc else fold_right_cps f t (f (get t i) acc) (i - 1)

let fold_left f acc t = if t.length = 0 then acc else fold_left_cps f acc t 0

let fold_right f t acc =
  let n = t.length in
  if n = 0 then acc else fold_right_cps f t acc (n - 1)

let to_string t f =
  let l = to_list t in
  "[| " ^ String.concat ~sep:"; " (List.map ~f l) ^ " |]"
