type t =
  | Word of string
  | Symbol of char
[@@deriving show, repr]

val parse : string -> (t * string) option
