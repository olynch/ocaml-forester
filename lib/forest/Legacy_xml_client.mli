open Forester_core

module T := Xml_tree
module P := Pure_html

module type Params = sig
  val host : string
  val home : string option
end

module type S = sig
  val route : Iri.t -> string
  val render_article : T.content T.article -> P.node

  val pp_xml : ?stylesheet: string -> Format.formatter -> T.content T.article -> unit
end

module Make (_: Params) (_: Forest.S) () : S
