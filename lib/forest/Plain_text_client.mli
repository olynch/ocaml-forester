open Forester_core

module T := Xml_tree

module type S =
sig
  val string_of_content : Xml_tree.content -> string
  val pp_content : Format.formatter -> Xml_tree.content -> unit
end

module Make (_ : Forest.S) : S