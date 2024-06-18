open Forester_core

module Gph : sig
  type t
  val safe_succ : t -> addr -> addr list
  val safe_pred : t -> addr -> addr list
end

module Topo : sig
  val fold : (addr -> 'a -> 'a) -> Gph.t -> 'a -> 'a
end

val build_import_graph : Code.tree list -> Gph.t
