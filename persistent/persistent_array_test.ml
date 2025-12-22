open Persistent_array

let () =
  let parr = make 10 0 in
  let arr = Array.make 10 0 in
  let parr1 = set parr 1 1 in
  arr.(1) <- 1;
  assert (List.equal ( = ) (Array.to_list arr) (to_list parr1));
  let parr2 = set parr1 2 2 in
  arr.(2) <- 2;
  assert (List.equal ( = ) (Array.to_list arr) (to_list parr2));
  assert (get parr1 2 = 0);
  assert (get parr2 2 = 2)
