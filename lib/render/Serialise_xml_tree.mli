open Forester_core

val route : root:string option -> addr -> string

module type I = sig val root : string option end

(** Each instance of this generative functor manages its own memo cache. *)
module Make (_ : I) () :
sig
  val pp : ?stylesheet:string -> Format.formatter -> Xml_tree.tree_ -> unit
end