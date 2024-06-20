module Rel :
sig
  type t
  [@@deriving show]

  val symbol : Symbol.t -> t

  val links : t
  val transclusion : t
  val authorship : t
  val contributorship : t
  val tags : t
  val taxa : t
end

type polarity =
  | Incoming
  | Outgoing
[@@deriving show]

type rel_query = polarity * Rel.t
[@@deriving show]

type 'addr t
[@@deriving show]

type ('addr, 'r) view =
  | Rel of polarity * Rel.t * 'addr
  | Tree_under of 'addr
  | Isect of 'r list
  | Union of 'r list
  | Complement of 'r
  | Isect_fam of 'r * rel_query
  | Union_fam of 'r * rel_query

val view : 'addr t -> ('addr, 'addr t) view

val map : ('a -> 'b) -> 'a t -> 'b t

(** {1 Smart constructors} *)
(** These smart constructors may optimise queries under the hood. *)

val tree_under : 'addr -> 'addr t
val isect : 'addr t list -> 'addr t
val union : 'addr t list -> 'addr t
val union_fam : 'addr t -> polarity -> Rel.t -> 'addr t
val isect_fam : 'addr t -> polarity -> Rel.t -> 'addr t
val rel : polarity -> Rel.t -> 'addr -> 'addr t
val complement : 'addr t -> 'addr t

open Base

val has_taxon : string -> addr t
val hereditary_contributors : addr -> addr t
val references : addr -> addr t
val context : addr -> addr t
val backlinks : addr -> addr t
val related : addr -> addr t
val contributions : addr -> addr t
