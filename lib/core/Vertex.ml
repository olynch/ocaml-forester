type t = Xml_tree.content Xml_tree.vertex
[@@deriving show]

let hash = Hashtbl.hash
let compare = compare
let equal = ( = )

let iri_of_vertex : _ Xml_tree.vertex -> Iri.t option = function
  | Iri_vertex iri -> Some iri
  | Content_vertex _ -> None
