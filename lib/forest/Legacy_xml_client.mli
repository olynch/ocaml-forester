open Forester_core

module T := Xml_tree
module P := Pure_html

module type Params = sig
  val root : string option
end

module type S = sig
  val route : addr -> string option
  val render_article : T.content T.article -> P.node

  val pp_xml : ?stylesheet: string -> Format.formatter -> T.content T.article -> unit
end

module Make (_: Params) (_: Forest.S) () : S
