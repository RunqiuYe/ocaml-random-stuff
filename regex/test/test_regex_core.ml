open Regex_core
open Regex_core.Regex

let () =
  let r = Epsilon in
  assert (Nfa.is_match r "");
  assert (not (Nfa.is_match r "a"));
  assert (not (Nfa.is_match r "aa"))

let () =
  let r = Concat (Epsilon, Char 'a') in
  assert (Nfa.is_match r "a");
  assert (not (Nfa.is_match r ""));
  assert (not (Nfa.is_match r "aa"))

let () =
  let r = Concat (Char 'a', Epsilon) in
  assert (Nfa.is_match r "a");
  assert (not (Nfa.is_match r ""));
  assert (not (Nfa.is_match r "aa"))

let () =
  let r = Concat (Char 'a', Epsilon) in
  assert (Nfa.is_match r "a");
  assert (not (Nfa.is_match r ""));
  assert (not (Nfa.is_match r "aa"))

let () =
  let r = Concat (Alt (Char 'a', Char 'b'), Char 'c') in
  assert (Nfa.is_match r "ac");
  assert (Nfa.is_match r "bc");
  assert (not (Nfa.is_match r "abc"));
  assert (not (Nfa.is_match r "bac"));
  assert (not (Nfa.is_match r "c"))

let () =
  let r = Star Epsilon in
  assert (Nfa.is_match r "");
  assert (not (Nfa.is_match r "a"))

let () =
  let r = Star (Alt (Char 'a', Char 'b')) in
  assert (Nfa.is_match r "");
  assert (Nfa.is_match r "a");
  assert (Nfa.is_match r "b");
  assert (Nfa.is_match r "abbaab");
  assert (not (Nfa.is_match r "abc"))

let () =
  let r = Star (Maybe (Char 'a')) in
  assert (Nfa.is_match r "");
  assert (Nfa.is_match r "aaaaa");
  assert (Nfa.is_match r "aaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (not (Nfa.is_match r "aaaaaaaaaaaaaab"))

let () =
  let r = Char 'a' in
  assert (Nfa.is_match r "a");
  assert (not (Nfa.is_match r "b"));
  assert (not (Nfa.is_match r "ab"))

let () =
  let r = Alt (Char 'a', Char 'b') in
  assert (Nfa.is_match r "a");
  assert (Nfa.is_match r "b");
  assert (not (Nfa.is_match r "aa"));
  assert (not (Nfa.is_match r "ab"));
  assert (not (Nfa.is_match r "bb"))

let () =
  let r = Maybe (Maybe (Char 'a')) in
  assert (Nfa.is_match r "");
  assert (Nfa.is_match r "a");
  assert (not (Nfa.is_match r "aa"))

let () =
  let r = Concat (Maybe (Char 'a'), Maybe (Char 'b')) in
  assert (Nfa.is_match r "");
  assert (Nfa.is_match r "a");
  assert (Nfa.is_match r "b");
  assert (Nfa.is_match r "ab");
  assert (not (Nfa.is_match r "ba"));
  assert (not (Nfa.is_match r "aa"))

let () =
  let r = Star (Char 'a') in
  assert (Nfa.is_match r "a");
  assert (not (Nfa.is_match r "b"));
  assert (not (Nfa.is_match r "ab"));
  assert (Nfa.is_match r "aaaaaaaaaaaa");
  assert (not (Nfa.is_match r "aaaaaaaabaaaaaa"))

let () =
  let r = Maybe (Char 'a') in
  assert (Nfa.is_match r "");
  assert (Nfa.is_match r "a");
  assert (not (Nfa.is_match r "b"));
  assert (not (Nfa.is_match r "aa"));
  assert (not (Nfa.is_match r "ba"))

let () =
  let r = Concat (Maybe (Char 'a'), Char 'a') in
  assert (Nfa.is_match r "aa");
  assert (Nfa.is_match r "a");
  assert (not (Nfa.is_match r ""))

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
  assert (Nfa.is_match r "aaaaaaaaaaaaaaaa");
  assert (Nfa.is_match r "aaaaaaaaaaaaa");
  assert (Nfa.is_match r "aaaaaaaa")

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
  assert (Nfa.is_match r "aaaaaaaaaaaaaaaa");
  assert (Nfa.is_match r "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (Nfa.is_match r "aaaaaaaaaaaaaaaaaaaaaaaaa")

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
  assert (Nfa.is_match r "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (
    Nfa.is_match r
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
  assert (not (Nfa.is_match r "aaaaaaaaaaaaaaaaaaaaaaaaa"));
  assert (
    not
      (Nfa.is_match r
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"));
  assert (Nfa.is_match r "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
