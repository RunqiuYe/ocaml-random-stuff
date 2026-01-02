open! Stdio

type state_node =
  | Undetermined
  | Accept
  | Direct of { match_c : char; out : state ref }
  | Split of { out1 : state ref; out2 : state ref }

and state = {
  state : state_node;
  mutable last_gen : int; (* avoid adding duplicates into current_states *)
}

type nfa_fragment = {
  start : state; (* start state of NFA *)
  outs : state ref list; (* a list of states with undetermined next states *)
}

type t = {
  current_states : state list; (* possible current state *)
  gen : int; (* number of generations, or number of character consumed *)
}

(* let print_state s =
  match s.state with
  | Undetermined -> printf "undetermined, "
  | Accept -> printf "accept, "
  | Direct { match_c; _ } -> printf "direct (%c, x), " match_c
  | Split _ -> printf "split (x, x), " *)

let rec build_frag (regex : Regex.t) : nfa_fragment =
  let make_state state = { state; last_gen = 0 } in
  match regex with
  | Epsilon -> { start = make_state Accept; outs = [] }
  | Char c ->
      let out = ref (make_state Undetermined) in
      let s = make_state (Direct { match_c = c; out }) in
      { start = s; outs = [ out ] }
  | Concat (r1, r2) ->
      let frag1, frag2 = (build_frag r1, build_frag r2) in
      List.iter (fun sref -> sref := frag2.start) frag1.outs;
      { start = frag1.start; outs = frag2.outs }
  | Alt (r1, r2) ->
      let frag1, frag2 = (build_frag r1, build_frag r2) in
      {
        start =
          make_state (Split { out1 = ref frag1.start; out2 = ref frag2.start });
        outs = frag1.outs @ frag2.outs;
      }
  | Star r ->
      let frag = build_frag r in
      let out2 = ref (make_state Undetermined) in
      let s = make_state (Split { out1 = ref frag.start; out2 }) in
      List.iter (fun sref -> sref := s) frag.outs;
      { start = s; outs = [ out2 ] }
  | Maybe r ->
      let frag = build_frag r in
      let out = ref (make_state Undetermined) in
      {
        start = make_state (Split { out1 = ref frag.start; out2 = out });
        outs = frag.outs @ [ out ];
      }

let rec get_next new_gen next_states acc =
  match next_states with
  | [] -> acc
  | s :: rest ->
      if s.last_gen = new_gen then get_next new_gen rest acc
      else (
        s.last_gen <- new_gen;
        match s.state with
        | Undetermined -> failwith "unreachable case"
        | Split { out1; out2 } -> get_next new_gen (!out1 :: !out2 :: rest) acc
        | _ -> get_next new_gen rest (s :: acc))

let of_regex (regex : Regex.t) : t =
  let make_state state = { state; last_gen = 0 } in
  let frag = build_frag regex in
  let acc = make_state Accept in
  List.iter (fun sref -> sref := acc) frag.outs;
  { current_states = get_next 1 [ frag.start ] []; gen = 1 }

let step c t =
  let new_gen = t.gen + 1 in
  let f s =
    match s.state with
    | Direct { match_c; out } -> if match_c = c then Some !out else None
    | Undetermined -> failwith "unreachable case"
    | _ -> None
  in
  let new_states = get_next new_gen (List.filter_map f t.current_states) [] in
  (* printf "current state: [ ";
  List.iter print_state new_states;
  printf " ]\n"; *)
  { current_states = new_states; gen = new_gen }

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
