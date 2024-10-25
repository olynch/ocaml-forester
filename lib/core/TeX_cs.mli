(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type t =
  | Word of string
  | Symbol of char

val pp : Format.formatter -> t -> unit
val show : t -> string

val t : t Repr.t

val parse : string -> (t * string) option
