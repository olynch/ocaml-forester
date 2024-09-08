open Forester_core

module T = Xml_tree

module type S = sig
  val plant_article : T.content T.article -> unit
  val get_article : iri -> T.content T.article option
  val get_expanded_title : ?scope: iri -> T.content T.frontmatter -> T.content
  val get_content_of_transclusion : T.content T.transclusion -> T.content
  val get_title_or_content_of_vertex : ?not_found: (iri -> T.content option) -> modifier: T.modifier -> T.content T.vertex -> T.content option
  val run_query : T.query -> Vertex_set.t
end

module Make (_: Forest_graphs.S) : S
