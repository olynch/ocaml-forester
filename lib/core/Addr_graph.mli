open Base

type t

val create : ?size:int -> unit -> t
val add_vertex : t -> addr -> unit
val add_edge : t -> addr -> addr -> unit
val safe_pred : t -> addr -> addr list
val safe_succ : t -> addr -> addr list
val mem_edge : t -> addr -> addr -> bool
val transitive_closure : ?reflexive:bool -> t -> t
