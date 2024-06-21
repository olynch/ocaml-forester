(** {1 Relation symbols}*)

module Rel :
sig
  type t = Symbol.t

  val links : t
  val transclusion : t
  val authors : t
  val contributors : t
  val tags : t
  val taxa : t
end

type rel = Rel.t

(** {1 Query modifiers} *)

(** Determines whether we are searching for edges into or out of the supplied vertex. *)
type polarity =
  | Incoming
  | Outgoing
[@@deriving show]

(** Determines whether we are querying a relation or its transitive closure. *)
type mode =
  | Edges
  | Paths
[@@deriving show]

(** {1 Query type} *)

(** The parameters of a relational query. *)
type rel_query = mode * polarity * Rel.t
[@@deriving show]

(** The abstract type of queries*)
type 'addr t
[@@deriving show]

(** One level of query syntax. *)
type ('addr, 'r) view =
  | Rel of rel_query * 'addr
  | Isect of 'r list
  | Union of 'r list
  | Complement of 'r
  | Isect_fam of 'r * rel_query
  | Union_fam of 'r * rel_query
[@@deriving show]

(** A view to expose a level of query syntax. *)
val view : 'addr t -> ('addr, 'addr t) view

val map : ('a -> 'b) -> 'a t -> 'b t

val query_t : 'a Repr.ty -> 'a t Repr.ty

(** {1 Smart constructors} *)
(** These smart constructors may optimise queries under the hood. *)

val isect : 'addr t list -> 'addr t
val union : 'addr t list -> 'addr t
val union_fam : 'addr t -> mode -> polarity -> Rel.t -> 'addr t
val isect_fam : 'addr t -> mode -> polarity -> Rel.t -> 'addr t
val rel : mode -> polarity -> Rel.t -> 'addr -> 'addr t
val complement : 'addr t -> 'addr t

(** {1 Built-in queries} *)
(** These convenience functions construct some useful queries using the smart constructors above. *)

open Base
val tree_under : 'addr -> 'addr t
val has_taxon : string -> addr t
val hereditary_contributors : addr -> addr t
val references : addr -> addr t
val context : addr -> addr t
val backlinks : addr -> addr t
val related : addr -> addr t
val contributions : addr -> addr t
