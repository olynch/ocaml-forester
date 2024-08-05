open Forester_prelude

type t =
  | Word of string
  | Symbol of char

let pp fmt =
  function
  | Word x -> Format.fprintf fmt "%s" x
  | Symbol x -> Format.fprintf fmt "%c" x

let show = Format.asprintf "%a" pp

let cs_symbol_rx = Str.regexp {|^[^A-Za-z]$|}
let cs_word_rx = Str.regexp {|^[A-Za-z]+$|}

let is_alpha c =
  let i = Char.code c in
  i >= 65 && i <= 90 || i >= 97 && i <= 122

let rec parse_word acc xs =
  match xs with
  | [] -> Word (String_util.implode_bwd acc), ""
  | x :: xs ->
    if is_alpha x then
      parse_word (Bwd.Snoc (acc, x)) xs
    else
      Word (String_util.implode_bwd acc), String_util.implode (x :: xs)

let parse input =
  match String_util.explode input with
  | x :: xs ->
    if is_alpha x then
      Some (parse_word (Bwd.Snoc (Bwd.Emp, x)) xs)
    else
      Some (Symbol x, String_util.implode xs)
  | [] -> None
