open Forester_core

module Make (F: Forest.S) = struct
  module PT = Plain_text_client.Make(F)(Plain_text_client.Default_params)
  module C = Types.Comparators(PT)

  let get_sorted_articles addrs =
    addrs
    |> Vertex_set.to_seq
    |> Seq.filter_map Vertex.iri_of_vertex
    |> Seq.filter_map F.get_article
    |> List.of_seq
    |> List.sort C.compare_article

  let get_all_articles () =
    Query.isect []
    |> F.run_query
    |> get_sorted_articles
end
