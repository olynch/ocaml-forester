(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(** {1 Base} *)

(** {2 IRIs} *)

type iri = Iri.t
val iri_t : Iri.t Repr.t
val pp_iri : Format.formatter -> Iri.t -> unit

module Iri_set: sig
  type t
  type elt = iri
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val empty : t
end

module Iri_map: sig
  type 'a t
  type key = iri
  val empty : 'a t
  val find_opt : key -> 'a t -> 'a option
  val add : key -> 'a -> 'a t -> 'a t
  val cardinal : 'a t -> int
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_list : 'a t -> (key * 'a) list
end

module Iri_tbl: sig
  type 'a t
  type key = iri

  val create : int -> 'a t
  val mem : 'a t -> key -> bool
  val find_opt : 'a t -> key -> 'a option
  val add : 'a t -> key -> 'a -> unit
  val length : 'a t -> int
  val to_seq_values : 'a t -> 'a Seq.t
  val to_seq_keys : 'a t -> key Seq.t
  val replace : 'a t -> key -> 'a -> unit
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

(** {2 Delimiters} *)

type delim = Braces | Squares | Parens
val pp_delim :
  Format.formatter -> delim -> unit
val show_delim : delim -> string
val delim_t : delim Repr.t
val delim_to_strings : delim -> string * string

(** {2 Variable binding} *)

type binding_strategy = Lazy | Strict
val pp_binding_strategy :
  Format.formatter ->
  binding_strategy ->
  unit
val show_binding_strategy : binding_strategy -> string
val binding_strategy_t : binding_strategy Repr.t
type 'a binding = binding_strategy * 'a
val pp_binding :
  (Format.formatter -> 'a -> unit) ->
  Format.formatter ->
  'a binding ->
  unit
val show_binding :
  (Format.formatter -> 'a -> unit) ->
  'a binding ->
  string
val binding_t : 'a Repr.t -> (binding_strategy * 'a) Repr.t

(** {2 Math modes} *)

type math_mode = Inline | Display

val pp_math_mode :
  Format.formatter ->
  math_mode ->
  unit

val show_math_mode : math_mode -> string

val math_mode_t : math_mode Repr.t

(** {2 Import visibility} *)

type visibility = Private | Public

val pp_visibility :
  Format.formatter ->
  visibility ->
  unit
val show_visibility : visibility -> string
val visibility_t : visibility Repr.t
