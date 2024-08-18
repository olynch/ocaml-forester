open Forester_core

type t
[@@deriving show, repr]

val named : Trie.path -> t
val fresh : unit -> t

val clone : t -> t
val compare : t -> t -> int
val repr : t Repr.t
