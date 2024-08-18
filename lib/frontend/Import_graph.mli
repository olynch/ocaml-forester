open Forester_core
open Forester_compiler

type t

val build_import_graph : Code.tree list -> t
val topo_fold : (addr -> 'a -> 'a) -> t -> 'a -> 'a
