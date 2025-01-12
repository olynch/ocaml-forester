(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler

module T = Types

val render_tree :
  dev: bool ->
  forest: State.t ->
  T.content T.article ->
  (string * Yojson.Safe.t) option

val render_trees :
  dev: bool ->
  forest: State.t ->
  Yojson.Safe.t
