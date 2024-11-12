(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)
open Forester_prelude
open Forester_core

module Unit_map: Map.S

type exports = Resolver.P.data Trie.Untagged.t

module Env: sig
  type t = exports Unit_map.t
  val empty : t
end

val builtins : (string list * Syn.node) list
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

val expand_dg :
  exports Unit_map.t ->
  Code.tree ->
  Reporter.Message.t Asai.Diagnostic.t list * (exports Unit_map.t * Syn.t)
