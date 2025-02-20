(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Types

type t = content vertex
[@@deriving show]

let clean = function
  | Content_vertex x -> Content_vertex x
  | Iri_vertex iri -> Iri_vertex (Iri.with_query iri (Iri.query iri))

let hash x = Hashtbl.hash (clean x)

let compare = compare
let equal = ( = )

let iri_of_vertex : _ Types.vertex -> Iri.t option = function
  | Iri_vertex iri -> Some iri
  | Content_vertex _ -> None
