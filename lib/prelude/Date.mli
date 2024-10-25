(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type t

val t : t Repr.ty

val drop_time : t -> t

val pp : Format.formatter -> t -> unit
val parse : string -> t option
val now : unit -> t

val compare : t -> t -> int

val year : t -> int
val month : t -> int option
val day : t -> int option
