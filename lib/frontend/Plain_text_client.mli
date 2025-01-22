(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T := Types

val string_of_content : T.content T.resource Forest.t -> Types.content -> string
val pp_content : T.content T.resource Forest.t -> Format.formatter -> Types.content -> unit
