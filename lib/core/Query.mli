(** {1 Relation symbols}*)

module Rel :
sig
  type t = string

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
