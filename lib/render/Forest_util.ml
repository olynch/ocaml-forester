open Forester_core

module Make (F : Forest.S) =
struct
  module PT = Plain_text_client.Make (F)
  module C = Xml_tree.Comparators (PT)

  let get_sorted_articles addrs =
    addrs
    |> Addr_set.to_seq
    |> Seq.filter_map F.get_article
    |> List.of_seq
    |> List.sort C.compare_article

  let get_all_articles () =
    Query.isect []
    |> Query.distill_expr
    |> F.run_query
    |> get_sorted_articles
end
