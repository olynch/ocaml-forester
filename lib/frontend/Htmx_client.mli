(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module T = Forester_core.Types

module P = Pure_html
module H = P.HTML
val route : Compiler.state -> Forester_core.iri -> string
val render_article : Compiler.state -> T.content T.article -> P.node

val render_content : Compiler.state -> T.content -> P.node list
