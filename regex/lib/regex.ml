type t = Epsilon | Char of char | Concat of t * t | Alt of t * t | Star of t
