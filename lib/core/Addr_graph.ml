open Base

module G = Graph.Imperative.Digraph.ConcreteBidirectional (Addr)
include G
include Graph.Oper.I (G)

let safe_succ g x =
  if mem_vertex g x then succ g x else []

let safe_fold_succ f g x acc =
  if mem_vertex g x then
    fold_succ f g x acc
  else
    acc

let safe_pred g x =
  if mem_vertex g x then pred g x else []
