(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

val pp : Format.formatter -> preamble: string -> body: string -> unit
val to_string : preamble: string -> body: string -> string
