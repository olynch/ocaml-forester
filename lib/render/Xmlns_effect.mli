module Make () : sig
  type xmlns_prefix = {prefix: string; xmlns: string}
  type 'b kont = added:xmlns_prefix option -> prefix:string -> 'b

  val find_xmlns_for_prefix : string -> string option
  val with_normalised_prefix : prefix:string -> xmlns:string option -> 'b kont -> 'b

  val run : reserved:xmlns_prefix list -> (unit -> 'a) -> 'a
end