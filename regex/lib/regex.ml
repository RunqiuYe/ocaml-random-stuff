type t =
  | Epsilon  (** match with only the empty string *)
  | Char of char  (** match with single character *)
  | Concat of t * t  (** concatenation of [r1] and [r2] *)
  | Alt of t * t  (** match with [r1] or [r2] *)
  | Star of t  (** match with zero or more [r] *)
  | Maybe of t  (** match with zero or one [r] *)
