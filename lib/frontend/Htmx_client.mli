(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module T = Forester_core.Types

module P = Pure_html
module H = P.HTML
module type S = sig
  val route : Forester_core.iri -> string
  val render_article : Compiler.state -> T.content T.article -> P.node
end
module type Params = sig val forest : Compiler.state end
module Make: functor (_: Params) () -> S
