(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type t
val t : t Repr.t

val pp : Format.formatter -> t -> unit

val compare : t -> t -> int

val parse_string : string -> t option
val parse_string_exn : string -> t

val year : t -> int
val month : t -> int option
val day : t -> int option

val now : unit -> t
val drop_time : t -> t
