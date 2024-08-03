open Base

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

(** Determines whether we are querying a relation or its reflexive-transitive closure. *)
type mode =
  | Edges
  | Paths
[@@deriving show]

(** {1 Query expression} *)

(** De Bruijn indices for bound variables, counting outward from the innermost binder. *)
type dbix = int
[@@deriving show]

(** Globally unique symbols for free variables. *)
type name = Symbol.t
[@@deriving show]

(** A variable in 'locally nameless' representation is either free or bound. *)
type lnvar =
  | F of name
  | B of dbix
[@@deriving show]

(** An address expression can be concrete, or it can be a variable. *)
type 'var addr_expr =
  | Addr of addr
  | Var of 'var
[@@deriving show]

(** We leave the type of binders abstract; but see {!body_of_binder} and {!make_binder}. *)
type 'a binder
[@@deriving show]

type 'var expr =
  | Rel of mode * polarity * Rel.t * 'var addr_expr
  | Isect of 'var expr list
  | Union of 'var expr list
  | Complement of 'var expr
  | Union_fam of 'var expr * 'var expr binder
  | Isect_fam of 'var expr * 'var expr binder
[@@deriving show]

val body_of_binder : dbix expr binder -> dbix expr
val make_binder : dbix expr -> dbix expr binder

val distill_expr : lnvar expr -> dbix expr
exception Distill of name

(** {2 Locally nameless operations and smart constructors} *)

val bind : Symbol.t -> lnvar expr -> lnvar expr binder
val unbind : lnvar expr binder -> Symbol.t * lnvar expr

val rel : mode -> polarity -> rel -> 'var addr_expr -> 'var expr
val isect : 'var expr list -> 'var expr
val union : 'var expr list -> 'var expr
val union_fam : lnvar expr -> Symbol.t -> lnvar expr -> lnvar expr
val isect_fam : lnvar expr -> Symbol.t -> lnvar expr -> lnvar expr
val isect_fam_rel : lnvar expr -> mode -> polarity -> rel -> lnvar expr
val union_fam_rel : lnvar expr -> mode -> polarity -> rel -> lnvar expr

val references : lnvar addr_expr -> lnvar expr
val context : lnvar addr_expr -> lnvar expr
val backlinks : lnvar addr_expr -> lnvar expr
val related : lnvar addr_expr -> lnvar expr
val contributions : lnvar addr_expr -> lnvar expr
val hereditary_contributors : lnvar addr_expr -> lnvar expr
