type state_node =
  | Undetermined
  | Accept of int
  | Direct of { match_c : char; out : state ref }
  | Split of state ref list

and state = {
  state : state_node;
  id : int; (* avoid adding duplicates into current_states *)
}

type nfa_fragment = {
  start : state; (* start state of NFA *)
  outs : state ref list; (* a list of states with undetermined next states *)
}

type t = { current_states : state list (* possible current state *) }
type compiled = state (* start state *)

module IntSet = Set.Make (Int)
(* used to keep track of already processed states *)

let make_state state id = { state; id }

(** [build r id_base] build nfa_fragment for [r] and new id_base *)
let rec build_frag (regex : Regex.t) (id_base : int) : nfa_fragment * int =
  match regex with
  | Epsilon ->
      (* use a split to represent an empty state with an undetermined out pointer *)
      let out = ref (make_state Undetermined 0) in
      let s = make_state (Split [ out ]) id_base in
      ({ start = s; outs = [ out ] }, id_base + 1)
  | Char c ->
      let out = ref (make_state Undetermined 0) in
      let s = make_state (Direct { match_c = c; out }) id_base in
      ({ start = s; outs = [ out ] }, id_base + 1)
  | Concat (r1, r2) ->
      let frag1, base1 = build_frag r1 id_base in
      let frag2, base2 = build_frag r2 base1 in
      List.iter (fun sref -> sref := frag2.start) frag1.outs;
      ({ start = frag1.start; outs = frag2.outs }, base2)
  | Alt (r1, r2) ->
      let frag1, base1 = build_frag r1 id_base in
      let frag2, base2 = build_frag r2 base1 in
      ( {
          start = make_state (Split [ ref frag1.start; ref frag2.start ]) base2;
          outs = frag1.outs @ frag2.outs;
        },
        base2 + 1 )
  | Star r ->
      let frag, base = build_frag r id_base in
      let out2 = ref (make_state Undetermined 0) in
      let s = make_state (Split [ ref frag.start; out2 ]) base in
      List.iter (fun sref -> sref := s) frag.outs;
      ({ start = s; outs = [ out2 ] }, base + 1)
  | Maybe r ->
      let frag, base = build_frag r id_base in
      let out = ref (make_state Undetermined 0) in
      ( {
          start = make_state (Split [ ref frag.start; out ]) base;
          outs = frag.outs @ [ out ];
        },
        base + 1 )
  | Multiple r -> 
    let frag, base = build_frag r id_base in 
    let out = ref (make_state Undetermined 0) in 
    let s = make_state (Split [ref frag.start; out]) base in
    List.iter (fun sref -> sref := s) frag.outs;
    ( {start = frag.start; outs = [out]}, base + 1)

let rec get_next next_states id_set acc =
  match next_states with
  | [] -> acc
  | s :: rest -> (
      if IntSet.exists (fun x -> x = s.id) id_set then get_next rest id_set acc
      else
        match s.state with
        | Undetermined -> failwith "unreachable case"
        | Split outs ->
            get_next (List.map ( ! ) outs @ rest) (IntSet.add s.id id_set) acc
        | _ -> get_next rest (IntSet.add s.id id_set) (s :: acc))

let of_regex regex id_base i =
  let frag, id_base = build_frag regex id_base in
  let acc = make_state (Accept i) (id_base + 1) in
  List.iter (fun sref -> sref := acc) frag.outs;
  frag.start, id_base + 2

let compile regex_list =
  let rec build_frags regex_list id_base i acc =
    match regex_list with
    | [] -> (id_base, acc)
    | r :: rest ->
        let start_state, id_base = of_regex r id_base i in
        build_frags rest id_base (i + 1) (start_state :: acc)
  in
  let id_base, start_states = build_frags regex_list 0 0 [] in
  make_state (Split (List.map ref start_states)) id_base

let start comp = { current_states = get_next [ comp ] IntSet.empty [] }

let step c t =
  let f s =
    match s.state with
    | Direct { match_c; out } -> if match_c = c then Some !out else None
    | Undetermined -> failwith "unreachable case"
    | _ -> None
  in
  let new_states =
    get_next (List.filter_map f t.current_states) IntSet.empty []
  in
  { current_states = new_states }

let is_dead t = List.is_empty t.current_states

let accept_patterns t =
  let pred s = match s.state with Accept i -> Some i | _ -> None in
  List.filter_map pred t.current_states |> List.sort Int.compare

let match_patterns t s =
  let chars = String.to_seq s |> List.of_seq in
  let rec consume chars t =
    match chars with [] -> accept_patterns t | c :: rest -> consume rest (step c t)
  in
  consume chars t

let match_prefix t s =
  let rec loop t s i best =
    if i = String.length s then best
    else if is_dead t then best
    else
      let t = step s.[i] t in
      let acc_p = accept_patterns t in
      if acc_p <> [] then loop t s (i + 1) (Some (List.hd acc_p, i + 1))
      else loop t s (i + 1) best
  in
  let acc_p = accept_patterns t in
  if acc_p <> [] (* case where the empty string is a match *) then
    loop t s 0 (Some (List.hd acc_p, 0))
  else loop t s 0 None