(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

val parse :
  ?stop_on_err: bool ->
  source: [`File of string | `String of Range.string_source] ->
  Lexing.lexbuf ->
  (Code.t, Reporter.diagnostic) result

val parse_file : string -> (Code.t, Reporter.diagnostic) result

val parse_document : Lsp.Text_document.t -> (Code.t, Reporter.diagnostic) result
