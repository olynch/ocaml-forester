(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T := Types
module P := Pure_html

val route : State.t -> Iri.t -> string
val render_article : State.t -> T.content T.article -> P.node
val render_content : State.t -> T.content -> P.node list
