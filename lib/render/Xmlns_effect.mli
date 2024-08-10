module Make () : sig
  type xmlns_prefix = {prefix: string; xmlns: string}
  val normalise_prefix : prefix:string -> xmlns:string option -> string
  val within_scope : (unit -> 'a) -> xmlns_prefix list * 'a
  val find_xmlns_for_prefix : string -> string option

  val run : reserved:xmlns_prefix list -> (unit -> 'a) -> 'a
end