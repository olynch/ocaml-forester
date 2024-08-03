type t
[@@deriving show]

val fresh : Trie.path -> t
val clone : t -> t
val compare : t -> t -> int
val repr : t Repr.t
