open Forester_core

module Make (_: sig val route : Iri.t -> string end) (_: Forest.S) : sig
  val render_trees :
    dev: bool ->
    host: string ->
    Xml_tree.content Xml_tree.article list ->
    Yojson.Basic.t
end
