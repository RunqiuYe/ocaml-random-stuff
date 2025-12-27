type regexp =
  | Char of char
  | Zero
  | One
  | Plus of regexp * regexp
  | Times of regexp * regexp
  | Star of regexp

let rec match_expr expr chars k =
  match expr with
  | Char a -> (
      match chars with [] -> false | c :: chars' -> a = c && k chars')
  | Zero -> false
  | One -> k chars
  | Times (r1, r2) -> match_expr r1 chars (fun chars' -> match_expr r2 chars' k)
  | Plus (r1, r2) -> match_expr r1 chars k || match_expr r2 chars k
  | Star r ->
      k chars || match_expr r chars (fun chars' -> match_expr (Star r) chars' k)
