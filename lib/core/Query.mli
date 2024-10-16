open Base

(** {1 Binary relation symbols} *)

type rel = string

(** {1 Query modifiers} *)

(** Determines whether we are searching for edges into or out of the supplied vertex. *)
type polarity =
  | Incoming
  | Outgoing

val pp_polarity : Format.formatter -> polarity -> unit
val polarity_t : polarity Repr.t

(** Determines whether we are querying a relation or its reflexive-transitive closure. *)
type mode =
  | Edges
  | Paths

val pp_mode : Format.formatter -> mode -> unit
val mode_t : mode Repr.t

(** {1 Query expression} *)

(** De Bruijn indices for bound variables, counting outward from the innermost binder. *)
type dbix = int

val pp_dbix : Format.formatter -> dbix -> unit
val dbix_t : dbix Repr.t

(** An address expression can be concrete, or it can be a variable. *)
type ('vertex, 'var) vertex_expr =
  | Vertex of 'vertex
  | Var of 'var

val pp_vertex_expr :
  (Format.formatter -> 'vertex -> unit) ->
  (Format.formatter -> 'var -> unit) ->
  Format.formatter ->
  ('vertex, 'var) vertex_expr ->
  unit

val vertex_expr_t :
  'vertex Repr.t ->
  'var Repr.t ->
  ('vertex, 'var) vertex_expr Repr.t

(** Don't use the constructor/destructor unless you know what you are doing! *)
type 'a binder = { body: 'a }
val binder_t : 'a Repr.t -> 'a binder Repr.t

type ('vertex, 'var) expr =
  | Rel of mode * polarity * rel * ('vertex, 'var) vertex_expr
  | Isect of ('vertex, 'var) expr list
  | Union of ('vertex, 'var) expr list
  | Complement of ('vertex, 'var) expr
  | Union_fam of ('vertex, 'var) expr * ('vertex, 'var) expr binder
  | Isect_fam of ('vertex, 'var) expr * ('vertex, 'var) expr binder

val pp_expr :
  (Format.formatter -> 'vertex -> unit) ->
  (Format.formatter -> 'var -> unit) ->
  Format.formatter ->
  ('vertex, 'var) expr ->
  unit

val expr_t : 'vertex Repr.t -> 'var Repr.t -> ('vertex, 'var) expr Repr.t

val rel : mode -> polarity -> rel -> ('vertex, 'var) vertex_expr -> ('vertex, 'var) expr
val isect : ('vertex, 'var) expr list -> ('vertex, 'var) expr
val union : ('vertex, 'var) expr list -> ('vertex, 'var) expr
val complement : ('vertex, 'var) expr -> ('vertex, 'var) expr

(** A variable in 'locally nameless' representation is either free or bound. *)
type 'name lnvar =
  | F of 'name
  | B of dbix

val pp_lnvar : (Format.formatter -> 'name -> unit) -> Format.formatter -> 'name lnvar -> unit

module type Name = sig
  type t
  val fresh : unit -> t
end

module Global_name: Name

module Locally_nameless (N: Name) : sig
  type 'vertex lnexpr = ('vertex, N.t lnvar) expr

  val distill : 'vertex lnexpr -> ('vertex, dbix) expr
  exception Distill of N.t

  val bind : N.t -> 'vertex lnexpr -> 'vertex lnexpr binder
  val union_fam : 'vertex lnexpr -> N.t -> 'vertex lnexpr -> 'vertex lnexpr
  val isect_fam : 'vertex lnexpr -> N.t -> 'vertex lnexpr -> 'vertex lnexpr

  val isect_fam_rel : 'vertex lnexpr -> mode -> polarity -> rel -> 'vertex lnexpr
  val union_fam_rel : 'vertex lnexpr -> mode -> polarity -> rel -> 'vertex lnexpr
end
