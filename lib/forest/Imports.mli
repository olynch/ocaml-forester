(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

type analysis_env = {
  graph: Forest_graph.t;
  follow: bool;
  forest: State.t
}

val build : State.t -> Forest_graph.t
val run_builder : ?root: iri -> analysis_env -> Forest_graph.t
val dependencies : Code.tree -> State.t -> Forest_graph.t
