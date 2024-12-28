(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T := Types

val string_of_content : Compiler.state -> Types.content -> string
val pp_content : Compiler.state -> Format.formatter -> Types.content -> unit
