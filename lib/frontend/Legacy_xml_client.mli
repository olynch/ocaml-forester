(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T := Types
module P := Pure_html

module type Params = sig
  val forest : Compiler.state
end

module type S = sig
  val route : Iri.t -> Compiler.state -> string
  val render_article : Compiler.state -> T.content T.article -> P.node

  val pp_xml : ?stylesheet: string -> Compiler.state -> Format.formatter -> T.content T.article -> unit
end

module Make (_: Params) () : S
