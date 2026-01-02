type state =
  | Undetermined
  | Accept
  | Direct of { match_c : char; out : state ref }
  | Split of { out1 : state ref; out2 : state ref }

type nfa_fragment = {
  start : state; (* start state of NFA *)
  outs : state ref list (* a list of states with undetermined next states *);
}

type t = { current_states : state list; accept_state : state }

let rec build_frag (regex : Regex.t) : nfa_fragment =
  match regex with
  | Epsilon -> { start = Accept; outs = [] }
  | Char c ->
      let out = ref Undetermined in
      let s = Direct { match_c = c; out } in
      { start = s; outs = [ out ] }
  | Concat (r1, r2) ->
      let frag1, frag2 = (build_frag r1, build_frag r2) in
      List.iter (fun sref -> sref := frag2.start) frag1.outs;
      { start = frag1.start; outs = frag2.outs }
  | Alt (r1, r2) ->
      let frag1, frag2 = (build_frag r1, build_frag r2) in
      {
        start = Split { out1 = ref frag1.start; out2 = ref frag2.start };
        outs = frag1.outs @ frag2.outs;
      }
  | Star r ->
      let frag = build_frag r in
      let out2 = ref Undetermined in
      let s = Split { out1 = ref frag.start; out2 } in
      List.iter (fun sref -> sref := s) frag.outs;
      { start = s; outs = [ out2 ] }

let build (regex : Regex.t) : t =
  let frag = build_frag regex in
  let acc = Accept in
  List.iter (fun sref -> sref := acc) frag.outs;
  { current_states = [ frag.start ]; accept_state = acc }

let rec get_next next_states acc =
  match next_states with
  | [] -> acc
  | s :: rest -> (
      match s with
      | Undetermined -> failwith "unreachable case"
      | Split { out1; out2 } -> get_next (!out1 :: !out2 :: rest) acc
      | _ -> get_next rest (s :: acc))

let step c t = 
  let f s = 
    match s with 
    | Direct {match_c ; out} -> if match_c = c then Some (!out) else None
    | Undetermined -> failwith "unreachable case"
    | _ -> None
  in
  { t with current_states = get_next (List.filter_map f t.current_states) []}

let is_accept t =
  let pred s =
    match s with
    | Accept -> true
    | Undetermined -> failwith "unreachable case"
    | _ -> false
  in
  List.exists pred t.current_states
