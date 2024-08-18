open Forester_core
type t = (Trie.path [@repr Repr.(list string)]) * int
[@@deriving repr]

let named path =
  path, Oo.id object end

let fresh () = named []

let clone (path, _) = named path

let pp fmt (sym, ix) =
  Format.fprintf fmt "%a@%i" Trie.pp_path sym ix

let show x = Format.asprintf "%a" pp x

let compare = compare

let repr : t Repr.t =
  Repr.pair (Repr.list Repr.string) Repr.int
