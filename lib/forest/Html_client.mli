open Forester_core

module T := Xml_tree
module P := Pure_html

module type S = sig
  val route : addr -> string
  val render_article : T.content T.article -> P.node
end

module type Params = sig
  val root : string option
end

module Make (_ : Params) (_ : Forest.S) () : S