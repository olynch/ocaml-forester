open Base

module type S = sig
  val register_addr : addr -> unit
  val get_all_addrs : unit -> Addr_set.t
  val get : Query.mode -> Query.Rel.t -> Addr_graph.t
  val add_edge : Query.Rel.t -> source:addr -> target:addr -> unit
end

module Make () : S
