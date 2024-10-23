open Forester_core
module T = Xml_tree

type t = {
  host: string;
  articles: T.content T.article list;
}
[@@deriving repr]

let render_trees ~host articles =
  Repr.to_json_string t { host; articles }
