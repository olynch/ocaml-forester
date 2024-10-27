(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

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
    let open Datalog_expr.Notation in
    let query = Datalog_expr.{ var = "X"; positives = [Builtin_relation.is_node @* [var "X"]]; negatives = [] } in
    query
    |> F.run_datalog_query
    |> get_sorted_articles
end
