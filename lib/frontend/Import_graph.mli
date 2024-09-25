open Forester_core
open Forester_compiler

include module type of Graph.Imperative.Digraph.Concrete(String)

val build : Code.tree list -> t
val topo_fold : (string -> 'a -> 'a) -> t -> 'a -> 'a
