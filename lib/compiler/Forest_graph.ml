(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module G = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
include G
include Graph.Oper.I(G)

module Reachability = Graph.Fixpoint.Make(G)(struct
  type vertex = G.E.vertex
  type edge = G.E.t
  type g = G.t
  type data = bool
  let direction = Graph.Fixpoint.Forward
  let equal = ( = )
  let join = ( || )
  let analyze _ = (fun x -> x)
end)

module Topo = Graph.Topological.Make(G)
let topo_fold = Topo.fold

let safe_succ g x =
  if mem_vertex g x then succ g x else []

let safe_pred g x =
  if mem_vertex g x then pred g x else []
