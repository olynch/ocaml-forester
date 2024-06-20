type t
[@@deriving show]

val fresh : Trie.path -> t
val compare : t -> t -> int
val repr : t Repr.t
