open Forester_prelude
open Forester_core

module Make (_ : sig val route : addr -> string option end) (_ : Forest.S) : sig
  val render_trees : dev:bool -> Xml_tree.content Xml_tree.article list -> Yojson.Basic.t
end
