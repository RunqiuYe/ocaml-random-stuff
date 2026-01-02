type state_node =
  | Undetermined
  | Accept
  | Direct of { match_c : char; out : state ref }
  | Split of { out1 : state ref; out2 : state ref }

and state = {
  state : state_node;
  id : int; (* avoid adding duplicates into current_states *)
}

type nfa_fragment = {
  start : state; (* start state of NFA *)
  outs : state ref list; (* a list of states with undetermined next states *)
}

type t = { current_states : state list (* possible current state *) }

module IntSet = Set.Make (Int)
(* used to keep track of already processed states *)

let make_state state id = { state; id }

(** [build r id_base] build nfa_fragment for [r] and new id_base *)
let rec build_frag (regex : Regex.t) (id_base : int) : nfa_fragment * int =
  match regex with
  | Epsilon ->
      (* use a split to represent an empty state with an undermined out pointer *)
      let out = ref (make_state Undetermined 0) in
      let s = make_state (Split { out1 = out; out2 = out }) id_base in
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
          start =
            make_state
              (Split { out1 = ref frag1.start; out2 = ref frag2.start })
              base2;
          outs = frag1.outs @ frag2.outs;
        },
        base2 + 1 )
  | Star r ->
      let frag, base = build_frag r id_base in
      let out2 = ref (make_state Undetermined 0) in
      let s = make_state (Split { out1 = ref frag.start; out2 }) base in
      List.iter (fun sref -> sref := s) frag.outs;
      ({ start = s; outs = [ out2 ] }, base + 1)
  | Maybe r ->
      let frag, base = build_frag r id_base in
      let out = ref (make_state Undetermined 0) in
      ( {
          start = make_state (Split { out1 = ref frag.start; out2 = out }) base;
          outs = frag.outs @ [ out ];
        },
        base + 1 )

let rec get_next next_states id_set acc =
  match next_states with
  | [] -> acc
  | s :: rest -> (
      if IntSet.exists (fun x -> x = s.id) id_set then get_next rest id_set acc
      else
        match s.state with
        | Undetermined -> failwith "unreachable case"
        | Split { out1; out2 } ->
            get_next (!out1 :: !out2 :: rest) (IntSet.add s.id id_set) acc
        | _ -> get_next rest (IntSet.add s.id id_set) (s :: acc))

let of_regex (regex : Regex.t) : t =
  let frag, base = build_frag regex 0 in
  let acc = make_state Accept (base + 1) in
  List.iter (fun sref -> sref := acc) frag.outs;
  { current_states = get_next [ frag.start ] IntSet.empty [] }

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

let is_accept t =
  let pred s =
    match s.state with
    | Accept -> true
    | Undetermined -> failwith "unreachable case"
    | _ -> false
  in
  List.exists pred t.current_states

let is_match regex s =
  let t = of_regex regex in
  let chars = String.to_seq s |> List.of_seq in
  let rec consume chars t =
    match chars with [] -> is_accept t | c :: rest -> consume rest (step c t)
  in
  consume chars t
