(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T = Forester_core.Types
module Dx = Forester_core.Datalog_expr

val iri_for_resource : 'a T.resource -> Forester_core__.Base.iri option

type env = (module Forest_graphs.S)

include Hashtbl.S with type key = iri

val legacy_query_engine : env -> (module Legacy_query_engine.S)

val run_datalog_query :
  (module Forest_graphs.S) -> (string, Forester_core.Vertex.t) Dx.query -> Forester_core.Vertex_set.t
