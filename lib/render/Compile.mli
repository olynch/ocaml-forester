open Forester_core

val run : (unit -> 'a) -> 'a
(** Initialises a cache for tree mainmatters in the given scope. *)

val compile_tree_top : Sem.tree -> Xml_tree.tree_
(** Must be called in the scope of {!run}. *)
