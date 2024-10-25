(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Base

type t = Types.content Types.vertex
include Set.OrderedType with type t := t
val equal : t -> t -> bool
val hash : t -> int

val pp : Format.formatter -> t -> unit
val show : t -> string

val iri_of_vertex : t -> iri option
