open Base

(** {1 Relation symbols}*)

module Rel :
sig
  type t = string

  val t : t Repr.ty
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
[@@deriving show, repr]

(** Determines whether we are querying a relation or its reflexive-transitive closure. *)
type mode =
  | Edges
  | Paths
[@@deriving show, repr]

(** {1 Query expression} *)

(** De Bruijn indices for bound variables, counting outward from the innermost binder. *)
type dbix = int
[@@deriving show, repr]

(** An address expression can be concrete, or it can be a variable. *)
type 'var addr_expr =
  | Addr of addr
  | Var of 'var
[@@deriving show, repr]

(** Don't use the constructor/destructor unless you know what you are doing! *)
type 'a binder = {body : 'a}
[@@deriving repr]

type 'var expr =
  | Rel of mode * polarity * Rel.t * 'var addr_expr
  | Isect of 'var expr list
  | Union of 'var expr list
  | Complement of 'var expr
  | Union_fam of 'var expr * 'var expr binder
  | Isect_fam of 'var expr * 'var expr binder
[@@deriving show]

val rel : mode -> polarity -> rel -> 'var addr_expr -> 'var expr
val isect : 'var expr list -> 'var expr
val union : 'var expr list -> 'var expr
val complement : 'var expr -> 'var expr

(** A variable in 'locally nameless' representation is either free or bound. *)
type 'name lnvar =
  | F of 'name
  | B of dbix
[@@deriving show]



module type Name = sig
  type t
  val fresh : unit -> t
end

module Global_name : Name

module Locally_nameless (N : Name) : sig
  type lnexpr = N.t lnvar expr

  val distill : lnexpr -> dbix expr
  exception Distill of N.t

  val bind : N.t -> lnexpr -> lnexpr binder
  val union_fam : lnexpr -> N.t -> lnexpr -> lnexpr
  val isect_fam : lnexpr -> N.t -> lnexpr -> lnexpr

  val has_taxon : string -> 'a expr
  val context : N.t lnvar addr_expr -> lnexpr
  val backlinks : N.t lnvar addr_expr -> lnexpr
  val related : N.t lnvar addr_expr -> lnexpr
  val contributions : N.t lnvar addr_expr -> lnexpr

  val isect_fam_rel : lnexpr -> mode -> polarity -> rel -> lnexpr
  val union_fam_rel : lnexpr -> mode -> polarity -> rel -> lnexpr

  val references : N.t lnvar addr_expr -> lnexpr
  val hereditary_contributors : N.t lnvar addr_expr -> lnexpr
end
