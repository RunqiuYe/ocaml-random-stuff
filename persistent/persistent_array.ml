(* Implementation of persistent array using sparse segment tree *)

type 'a tree = 'a data ref
and 'a data = Leaf of 'a | Node of 'a tree * 'a tree

type 'a t = int * 'a tree

let rec build l r f =
  if l = r then ref (Leaf (f l))
  else
    let mid = l + ((r - l) / 2) in
    ref (Node (build l mid f, build (mid + 1) r f))

let rec query tree l r i =
  if l = r then
    match !tree with Leaf x -> x | _ -> failwith "unreachable case"
  else
    match !tree with
    | Node (ltree, rtree) ->
        let mid = l + ((r - l) / 2) in
        if i <= mid then query ltree l mid i else query rtree (mid + 1) r i
    | _ -> failwith "unreachable case"

let rec update tree l r i x =
  if l = r then ref (Leaf x)
  else
    match !tree with
    | Node (ltree, rtree) ->
        let mid = l + ((r - l) / 2) in
        if i <= mid then ref (Node (update ltree l mid i x, rtree))
        else ref (Node (ltree, update rtree (mid + 1) r i x))
    | _ -> failwith "unreachable case"

let make n x = (n, build 0 (n - 1) (fun _ -> x))
let init n f = (n, build 0 (n - 1) f)

let length t =
  let n, _ = t in
  n

let get t i =
  let n, tree = t in
  query tree 0 (n - 1) i

let set t i x =
  let n, tree = t in
  (n, update tree 0 (n - 1) i x)

let to_list t =
  let n, _ = t in
  List.init n (fun i -> get t i)

let iter f t =
  let n, _ = t in
  for i = 0 to n - 1 do
    f (get t i)
  done

let iteri f t = 
  let n, _ = t in 
  for i = 0 to n - 1 do 
    f i (get t i)
  done

let rec fold_left_cps f acc t i = 
  let n, _ = t in 
  if i = n - 1 then f acc (get t (n - 1))
  else fold_left_cps f (f acc (get t i)) t (i + 1)

let rec fold_right_cps f t acc i = 
  if i = 0 then f (get t 0) acc 
  else fold_right_cps f t (f (get t i) acc) (i - 1)

let fold_left f acc t = 
  fold_left_cps f acc t 0

let fold_right f t acc = 
  let n, _ = t in 
  fold_right_cps f t acc (n - 1)
  