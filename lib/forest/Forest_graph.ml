(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
include G
include Graph.Oper.I(G)

let safe_succ g x =
  if mem_vertex g x then succ g x else []

let safe_pred g x =
  if mem_vertex g x then pred g x else []
