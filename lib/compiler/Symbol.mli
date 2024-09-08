open Forester_core

type t

val pp : Format.formatter -> t -> unit
val show : t -> string

val t : t Repr.t

val named : Trie.path -> t
val fresh : unit -> t

val clone : t -> t
val compare : t -> t -> int
val repr : t Repr.t
