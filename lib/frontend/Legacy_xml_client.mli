(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T := Types
module P := Pure_html

val route : Compiler.state -> Iri.t -> string
val render_article : Compiler.state -> T.content T.article -> P.node
val render_content : Compiler.state -> T.content -> P.node list
