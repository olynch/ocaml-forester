open Forester_core

module T = Xml_tree

module type S = sig
  type resource =
    | Article of T.content T.article
    | Asset of iri

  val plant_resource : resource -> unit
  val get_resource : iri -> resource option
  val get_article : iri -> T.content T.article option
  val get_expanded_title : ?scope: iri -> ?flags: T.title_flags -> T.content T.frontmatter -> T.content
  val get_content_of_transclusion : T.content T.transclusion -> T.content
  val get_title_or_content_of_vertex : ?not_found: (iri -> T.content option) -> modifier: T.modifier -> T.content T.vertex -> T.content option
  val run_datalog_query : (string, Vertex.t) Datalog_expr.query -> Vertex_set.t
  val run_query : T.query -> Vertex_set.t
end

module Make (_: Forest_graphs.S) : S
