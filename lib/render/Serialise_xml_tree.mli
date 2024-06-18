open Forester_core

val run : (unit -> 'a) -> 'a
(** Initialises a cache for tree mainmatters in the given scope. *)

val pp : ?stylesheet:string -> Format.formatter -> Xml_tree.tree_ -> unit
(** Must be called within the scope of {!run}. *)