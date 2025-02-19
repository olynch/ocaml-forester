open Base

type 'a t
type key = iri

val create : int -> 'a t
val mem : 'a t -> key -> bool
val find_opt : 'a t -> key -> 'a option
val add : 'a t -> key -> 'a -> unit
val length : 'a t -> int
val to_seq_values : 'a t -> 'a Seq.t
val to_seq_keys : 'a t -> key Seq.t
val replace : 'a t -> key -> 'a -> unit
val iter : (key -> 'a -> unit) -> 'a t -> unit