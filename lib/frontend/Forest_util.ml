(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module Make (F: sig val forest : Compiler.state end) = struct
  module PT = Plain_text_client.Make(struct
    let route = Iri.to_uri
    let forest = F.forest
  end)
  module C = Types.Comparators(PT)

  let get_sorted_articles forest addrs =
    addrs
    |> Vertex_set.to_seq
    |> Seq.filter_map Vertex.iri_of_vertex
    |> Seq.filter_map (fun iri -> Compiler.get_article iri forest)
    |> List.of_seq
    |> List.sort C.compare_article
end
