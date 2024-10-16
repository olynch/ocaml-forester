open Forester_core

module type S = sig
  val run_query : Xml_tree.query -> Vertex_set.t
end

module Make (_: Forest_graphs.S) : S
