open Forester_core

module type S = sig
  val run_query : Query.dbix Query.expr -> Addr_set.t
end

module Make (_ : Forest_graphs.S) : S
