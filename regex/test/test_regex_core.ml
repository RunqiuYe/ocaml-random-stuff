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

let () =
  let m = Maybe (Char 'a') in
  let a = Char 'a' in
  let nm =
    Concat
      ( Concat (Concat (m, m), Concat (m, m)),
        Concat (Concat (m, m), Concat (m, m)) )
  in
  let na =
    Concat
      ( Concat (Concat (a, a), Concat (a, a)),
        Concat (Concat (a, a), Concat (a, a)) )
  in
  let r = Concat (nm, na) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaa");
  assert (Nfa.is_match nfa "aaaaaaaaaaaaa");
  assert (Nfa.is_match nfa "aaaaaaaa")

let () =
  let m = Maybe (Char 'a') in
  let a = Char 'a' in
  let nm =
    Concat
      ( Concat
          ( Concat (Concat (m, m), Concat (m, m)),
            Concat (Concat (m, m), Concat (m, m)) ),
        Concat
          ( Concat (Concat (m, m), Concat (m, m)),
            Concat (Concat (m, m), Concat (m, m)) ) )
  in
  let na =
    Concat
      ( Concat
          ( Concat (Concat (a, a), Concat (a, a)),
            Concat (Concat (a, a), Concat (a, a)) ),
        Concat
          ( Concat (Concat (a, a), Concat (a, a)),
            Concat (Concat (a, a), Concat (a, a)) ) )
  in
  let r = Concat (nm, na) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaa");
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaaaaaaaaaaa")

let () =
  let m = Maybe (Char 'a') in
  let a = Char 'a' in
  let nm =
    Concat
      ( Concat
          ( Concat
              ( Concat (Concat (m, m), Concat (m, m)),
                Concat (Concat (m, m), Concat (m, m)) ),
            Concat
              ( Concat (Concat (m, m), Concat (m, m)),
                Concat (Concat (m, m), Concat (m, m)) ) ),
        Concat
          ( Concat
              ( Concat (Concat (m, m), Concat (m, m)),
                Concat (Concat (m, m), Concat (m, m)) ),
            Concat
              ( Concat (Concat (m, m), Concat (m, m)),
                Concat (Concat (m, m), Concat (m, m)) ) ) )
  in
  let na =
    Concat
      ( Concat
          ( Concat
              ( Concat (Concat (a, a), Concat (a, a)),
                Concat (Concat (a, a), Concat (a, a)) ),
            Concat
              ( Concat (Concat (a, a), Concat (a, a)),
                Concat (Concat (a, a), Concat (a, a)) ) ),
        Concat
          ( Concat
              ( Concat (Concat (a, a), Concat (a, a)),
                Concat (Concat (a, a), Concat (a, a)) ),
            Concat
              ( Concat (Concat (a, a), Concat (a, a)),
                Concat (Concat (a, a), Concat (a, a)) ) ) )
  in
  let r = Concat (nm, na) in
  let nfa = Nfa.of_regex r in
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (
    Nfa.is_match nfa
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (not (Nfa.is_match nfa "aaaaaaaaaaaaaaaaaaaaaaaaa"));
  assert (
    not
      (Nfa.is_match nfa
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"));
  assert (Nfa.is_match nfa "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
