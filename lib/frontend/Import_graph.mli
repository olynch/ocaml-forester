open Forester_core
open Forester_compiler

include module type of Graph.Imperative.Digraph.Concrete (Addr)

val build_import_graph : Code.tree list -> t
val topo_fold : (addr -> 'a -> 'a) -> t -> 'a -> 'a
