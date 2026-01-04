open Regex_core
open Regex_core.Regex

let () =
  let r = Epsilon in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "");
  assert (not (Nfa.is_match nfa "a"));
  assert (not (Nfa.is_match nfa "aa"))

let () =
  let r = Concat (Epsilon, Char 'a') in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "a");
  assert (not (Nfa.is_match nfa ""));
  assert (not (Nfa.is_match nfa "aa"))

let () =
  let r = Concat (Char 'a', Epsilon) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "a");
  assert (not (Nfa.is_match nfa ""));
  assert (not (Nfa.is_match nfa "aa"))

let () =
  let r = Concat (Char 'a', Epsilon) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "a");
  assert (not (Nfa.is_match nfa ""));
  assert (not (Nfa.is_match nfa "aa"))

let () =
  let r = Concat (Alt (Char 'a', Char 'b'), Char 'c') in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "ac");
  assert (Nfa.is_match nfa "bc");
  assert (not (Nfa.is_match nfa "abc"));
  assert (not (Nfa.is_match nfa "bac"));
  assert (not (Nfa.is_match nfa "c"))

let () =
  let r = Star Epsilon in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "");
  assert (not (Nfa.is_match nfa "a"))

let () =
  let r = Star (Alt (Char 'a', Char 'b')) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "");
  assert (Nfa.is_match nfa "a");
  assert (Nfa.is_match nfa "b");
  assert (Nfa.is_match nfa "abbaab");
  assert (not (Nfa.is_match nfa "abc"))

let () =
  let r = Star (Maybe (Char 'a')) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "");
  assert (Nfa.is_match nfa "aaaaa");
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (not (Nfa.is_match nfa "aaaaaaaaaaaaaab"))

let () =
  let r = Char 'a' in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "a");
  assert (not (Nfa.is_match nfa "b"));
  assert (not (Nfa.is_match nfa "ab"))

let () =
  let r = Alt (Char 'a', Char 'b') in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "a");
  assert (Nfa.is_match nfa "b");
  assert (not (Nfa.is_match nfa "aa"));
  assert (not (Nfa.is_match nfa "ab"));
  assert (not (Nfa.is_match nfa "bb"))

let () =
  let r = Maybe (Maybe (Char 'a')) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "");
  assert (Nfa.is_match nfa "a");
  assert (not (Nfa.is_match nfa "aa"))

let () =
  let r = Concat (Maybe (Char 'a'), Maybe (Char 'b')) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "");
  assert (Nfa.is_match nfa "a");
  assert (Nfa.is_match nfa "b");
  assert (Nfa.is_match nfa "ab");
  assert (not (Nfa.is_match nfa "ba"));
  assert (not (Nfa.is_match nfa "aa"))

let () =
  let r = Star (Char 'a') in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "a");
  assert (not (Nfa.is_match nfa "b"));
  assert (not (Nfa.is_match nfa "ab"));
  assert (Nfa.is_match nfa "aaaaaaaaaaaa");
  assert (not (Nfa.is_match nfa "aaaaaaaabaaaaaa"))

let () =
  let r = Maybe (Char 'a') in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "");
  assert (Nfa.is_match nfa "a");
  assert (not (Nfa.is_match nfa "b"));
  assert (not (Nfa.is_match nfa "aa"));
  assert (not (Nfa.is_match nfa "ba"))

let () =
  let r = Concat (Maybe (Char 'a'), Char 'a') in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "aa");
  assert (Nfa.is_match nfa "a");
  assert (not (Nfa.is_match nfa ""))

let concat_list regexps =
  List.fold_left (fun r1 r2 -> Concat (r1, r2)) Epsilon regexps

let () =
  let m_list = List.init 8 (fun _ -> Maybe (Char 'a')) in
  let a_list = List.init 8 (fun _ -> Char 'a') in
  let nm = concat_list m_list in
  let na = concat_list a_list in
  let r = Concat (nm, na) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaa");
  assert (Nfa.is_match nfa "aaaaaaaaaaaaa");
  assert (Nfa.is_match nfa "aaaaaaaa")

let () =
  let m_list = List.init 16 (fun _ -> Maybe (Char 'a')) in
  let a_list = List.init 16 (fun _ -> Char 'a') in
  let nm = concat_list m_list in
  let na = concat_list a_list in
  let r = Concat (nm, na) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaa");
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaaaaaaaaaaa")

let () =
  let m_list = List.init 128 (fun _ -> Maybe (Char 'a')) in
  let a_list = List.init 128 (fun _ -> Char 'a') in
  let nm = concat_list m_list in
  let na = concat_list a_list in
  let r = Concat (nm, na) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa (String.make 128 'a'));
  assert (Nfa.is_match nfa (String.make 256 'a'));
  assert (Nfa.is_match nfa (String.make 157 'a'));
  assert (not (Nfa.is_match nfa (String.make 300 'a')))

let () =
  let r = Multiple (Concat (Char 'a', Char 'b')) in
  let nfa = Nfa.of_regex r in
  assert (not (Nfa.is_match nfa "aaab"));
  assert (Nfa.is_match nfa "ababab");
  assert (not (Nfa.is_match nfa ""))

let () =
  let r = Star (Char 'a') in
  let nfa = Nfa.of_regex r in
  assert (Nfa.match_prefix nfa "aaaaab" = Some 5);
  assert (Nfa.match_prefix nfa "bbbbb" = Some 0);
  assert (Nfa.match_prefix nfa "abbbbb" = Some 1)

let () =
  let r = Maybe (Concat (Char 'a', Char 'b')) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.match_prefix nfa "aaab" = Some 0);
  assert (Nfa.match_prefix nfa "ababab" = Some 2);
  assert (Nfa.match_prefix nfa "" = Some 0)

let () =
  let r = Multiple (Concat (Char 'a', Char 'b')) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.match_prefix nfa "aaab" = None);
  assert (Nfa.match_prefix nfa "ababab" = Some 6);
  assert (Nfa.match_prefix nfa "" = None)

let () = 
  let rs = [ Star (Char 'a') ] in
  let nfa = Multi_nfa.compile rs |> Multi_nfa.start in
  assert (Multi_nfa.match_patterns nfa "aaaaa" <> [])

let () =
  let rs = [ Star (Char 'a') ] in
  let nfa = Multi_nfa.compile rs |> Multi_nfa.start in
  assert (Multi_nfa.match_prefix nfa "aaaaab" = Some (0, 5));
  assert (Multi_nfa.match_prefix nfa "bbbbb" = Some (0, 0));
  assert (Multi_nfa.match_prefix nfa "abbbbb" = Some (0, 1))

