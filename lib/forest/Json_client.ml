open Forester_core
module T = Xml_tree

type t = {
  host: string;
  resources: T.content T.resource list;
}
[@@deriving repr]

let render ~host resources =
  Repr.to_json_string t { host; resources }
