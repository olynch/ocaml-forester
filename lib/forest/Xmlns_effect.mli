open Forester_core

module Make () : sig
  type xmlns_attr = {prefix: string; xmlns: string}
  val normalise_qname : xml_qname -> xml_qname
  val within_scope : (unit -> 'a) -> xmlns_attr list * 'a
  val find_xmlns_for_prefix : string -> string option

  val run : reserved:xmlns_attr list -> (unit -> 'a) -> 'a
end