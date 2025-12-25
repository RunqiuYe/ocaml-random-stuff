open Persistent_array

let rec insert arr i v =
  if i = 0 then set arr 0 v
  else
    let x = get arr (i - 1) in
    if x < v then set arr i v else insert (set arr i x) (i - 1) v

let insertion_sort arr =
  let n = length arr in
  let rec loop arr i =
    if i >= n then arr else loop (insert arr i (get arr i)) (i + 1)
  in
  loop arr 1

let sum = fold_left ( + ) 0

let test n =
  let arr = init n (fun i -> n - 1 - i) in
  let b = insertion_sort arr in
  for i = 0 to n - 1 do
    assert (get b i = i && get arr i = n - 1 - i)
  done;
  let s = n * (n - 1) / 2 in
  assert (sum arr = s && sum b = s)

let () =
  for n = 0 to 5 do
    test n
  done

(** fold over an array [a] while accessing to another version [b] **)
let arr = init 3 (fun i -> i)
let () = assert (sum arr = 3)
let b = set arr 2 3
let () = assert (get b 2 = 3)
let () = assert (sum b = 4)
let () = assert (fold_left (fun s x -> s + x + get b 2) 0 arr = 12)
