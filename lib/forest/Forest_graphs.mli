open Forester_core

module type S = sig
  val dl_db : Datalog_engine.db
  val register_iri : Iri.t -> unit
  val get_all_vertices : unit -> Vertex_set.t
  val get_rel : Query.mode -> Query.rel -> Forest_graph.t
  val add_edge : Query.rel -> source: Vertex.t -> target: Vertex.t -> unit
end

module Make () : S
