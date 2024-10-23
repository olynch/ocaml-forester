type t = Types.content Types.vertex
[@@deriving show]

let hash = Hashtbl.hash
let compare = compare
let equal = ( = )

let iri_of_vertex : _ Types.vertex -> Iri.t option = function
  | Iri_vertex iri -> Some iri
  | Content_vertex _ -> None
