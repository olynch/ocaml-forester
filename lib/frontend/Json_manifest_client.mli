(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
module T = Types

val render_tree :
  dev: bool ->
  forest: Compiler.state ->
  T.content T.article ->
  (string * Yojson.Safe.t) option

val render_trees :
  dev: bool ->
  forest: Compiler.state ->
  Yojson.Safe.t
