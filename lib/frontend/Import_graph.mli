open Forester_core
open Forester_compiler

include module type of Graph.Imperative.Digraph.Concrete(Iri_hash)

val build : host: string option -> Code.tree list -> t
val topo_fold : (iri -> 'a -> 'a) -> t -> 'a -> 'a
