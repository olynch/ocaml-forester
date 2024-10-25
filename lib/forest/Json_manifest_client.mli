(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module Make (_: sig val route : Iri.t -> string end) (_: Forest.S) : sig
  val render_trees :
    dev: bool ->
    host: string ->
    Types.content Types.article list ->
    Yojson.Basic.t
end
