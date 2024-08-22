open Forester_core

module T = Xml_tree

module type S = sig
  val plant_article : T.content T.article -> unit
  val get_article : addr -> T.content T.article option
  val get_expanded_title : ?scope:addr option -> T.content T.frontmatter -> T.content
  val get_content_of_transclusion : T.content T.transclusion -> T.content
  val run_query : Query.dbix Query.expr -> Addr_set.t
end

module Make (_ : Forester_graphs.S) : S
