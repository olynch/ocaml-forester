open Forester_core

type t
[@@deriving show]

val named : Trie.path -> t
val fresh : unit -> t

val clone : t -> t
val compare : t -> t -> int
val repr : t Repr.t
