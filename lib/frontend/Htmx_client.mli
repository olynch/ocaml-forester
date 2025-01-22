(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T = Forester_core.Types

module P = Pure_html
module H = P.HTML

type query = {
  query: (string, T.content T.vertex) Forester_core.Datalog_expr.query;
}
val query_t : query Repr.t

val route : State.t -> Forester_core.iri -> string
val render_article : State.t -> T.content T.article -> P.node

val render_content : State.t -> T.content -> P.node list
val render_frontmatter : State.t -> T.content T.frontmatter -> P.node

val render_query_result : State.t -> Vertex_set.t -> P.node list
