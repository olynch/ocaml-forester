(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_compiler

(** An implementation of the {{: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/} Microsoft Language Server Protocol } for forester. *)

(** Analysis of the {{!type:Forester_compiler.Code.t}concrete syntax}*)
module Analysis = Analysis

(** The event loop implementation and request handlers*)
module Server = Lsp_server

(** Start the language server *)
val start :
  env: Eio_unix.Stdenv.base ->
  config: Config.t ->
  unit
