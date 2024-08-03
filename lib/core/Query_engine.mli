open Base

module type S = sig
  val run_query : Query.dbix Query.expr -> Addr_set.t
end

module Make (_ : Forester_graphs.S) : S
