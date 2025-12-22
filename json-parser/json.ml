open Core

type value = [
  | `Assoc of (string * value) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of value list
  | `Null
  | `String of string
]

let rec output_value value =
  match value with
  | `Assoc obj  -> print_assoc obj
  | `List l     -> print_list l
  | `String s   -> printf "\"%s\"" s
  | `Int i      -> printf "%d" i
  | `Float x    -> printf "%f" x
  | `Bool true  -> printf "true"
  | `Bool false -> printf "false"
  | `Null       -> printf "null"

and print_assoc obj =
  printf "{ ";
  let sep = ref "" in
  List.iter obj ~f:(fun (key, value) ->
      printf "%s\"%s\": " !sep key;
      output_value value;
      sep := ", ");
  printf " }"

and print_list arr =
  printf "[";
  List.iteri arr ~f:(fun i v ->
      if i > 0 then
        printf ", ";
      output_value v);
  printf "]"