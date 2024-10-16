type t = Xml_tree.content Xml_tree.vertex
[@@deriving show]

let hash = Hashtbl.hash
let compare = compare
let equal = ( = )

let iri_of_vertex : _ Xml_tree.vertex -> Iri.t option = function
  | Iri_vertex iri -> Some iri
  | Content_vertex _ -> None

module D = Datalog_engine

let tbl = Hashtbl.create 1000

let pack vtx : D.symbol =
  let encoding = D.StringSymbol.make @@ string_of_int @@ hash vtx in
  begin
    match Hashtbl.find_opt tbl encoding with
    | Some _ -> ()
    | None ->
      Hashtbl.add tbl encoding vtx
  end;
  encoding

let unpack str = Hashtbl.find_opt tbl str
