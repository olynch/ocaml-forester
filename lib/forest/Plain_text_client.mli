(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T := Types

module type Params = sig
  val route : iri -> string
end

module Default_params: Params

module type S = sig
  val string_of_content : Types.content -> string
  val pp_content : Format.formatter -> Types.content -> unit
end

module Make (_: Forest.S) (_: Params) : S
