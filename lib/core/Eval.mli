open Forester_prelude
open Base

module Make () :
sig
  include Query_engine.S
  val eval_tree : addr:addr -> source_path:string option -> Syn.tree -> Sem.tree * Sem.tree list
end
