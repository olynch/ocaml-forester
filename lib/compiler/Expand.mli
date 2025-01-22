(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module Unit_map : Map.S with type key = string

type exports = (Resolver.P.data, Asai.Range.t option) Trie.t

module Env : sig
  type t = exports Unit_map.t
  val empty : t
end

val builtins : (string list * Syn.node) list

module Builtins :
sig
  module Transclude :
  sig
    val expanded_sym : Symbol.t
    val show_heading_sym : Symbol.t
    val show_metadata_sym : Symbol.t
    val toc_sym : Symbol.t
    val numbered_sym : Symbol.t
  end
end

val suggestions : string list -> ('a, 'b) Trie.t -> (Trie.path * 'a * int) list

val expand_tree :
  ?quit_on_error: bool ->
  Env.t ->
  Code.tree ->
  Reporter.diagnostic list * exports Unit_map.t * Syn.t
