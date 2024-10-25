(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module Env: sig
  type t
  val empty : t
end

val expand_tree : Env.t -> Code.tree -> Env.t * Syn.tree

module Builtins:
sig
  module Transclude:
  sig
    val expanded_sym : Symbol.t
    val show_heading_sym : Symbol.t
    val show_metadata_sym : Symbol.t
    val toc_sym : Symbol.t
    val numbered_sym : Symbol.t
  end
end
