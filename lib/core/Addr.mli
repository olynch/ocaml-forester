type t =
  | User_addr of string
  (** The address of a tree that can be referenced from user text. *)

  | Machine_addr of int
  (** The address of an tree with unstable address.*)

  | Hash_addr of string
  (** The address of a content-addressed tree. *)

  | Anon
[@@deriving repr]

val pp : Format.formatter -> t -> unit

include Map.OrderedType with type t := t
include Graph.Sig.COMPARABLE with type t := t

val is_user_addr : t -> bool
val user_addr : string -> t

val hash_addr : string -> t
val fresh : unit -> t
val anon : t