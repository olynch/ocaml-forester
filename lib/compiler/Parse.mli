(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

include module type of Forester_parser.Parse

val parse_document :
  Lsp.Text_document.t ->
  (Forester_parser.Code.t, Forester_core.Reporter.diagnostic) result

val parse_file :
  string ->
  (Forester_parser.Code.t, Forester_core.Reporter.diagnostic) result

