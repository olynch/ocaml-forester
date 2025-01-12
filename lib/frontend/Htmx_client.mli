(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_compiler 

module T = Forester_core.Types

module P = Pure_html
module H = P.HTML
val route : State.t -> Forester_core.iri -> string
val render_article : State.t -> T.content T.article -> P.node

val render_content : State.t -> T.content -> P.node list
