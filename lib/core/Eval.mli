open Forester_prelude
open Base

module Make () :
sig
  val eval_tree : addr:addr -> source_path:string option -> Syn.tree -> Sem.tree * Sem.tree list
  val run_query : addr Query.t -> Addr_set.t
end