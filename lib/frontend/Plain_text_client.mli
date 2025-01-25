(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T := Types

val string_of_content :
  forest: T.content T.resource Forest.t ->
  router: (iri -> string) ->
  Types.content ->
  string

val pp_content :
  forest: T.content T.resource Forest.t ->
  router: (iri -> string) ->
  Format.formatter ->
  Types.content ->
  unit
