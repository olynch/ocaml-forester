open Forester_prelude
open Forester_core

module type I =
sig
  val root : string option
  val trees : Sem.tree Addr_map.t
  val run_query : Query.dbix Query.expr -> Addr_set.t
  val last_changed : addr -> Date.t option
  val enqueue_latex : name:string -> preamble:string -> source:string -> unit
end

(** Each instance of this generative functor manages its own memo cache. *)
module Make (_ : I) () :
sig
  val compile_tree : Sem.tree -> Xml_tree.tree_
end
