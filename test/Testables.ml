(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler
open Alcotest

type 'a bwd = 'a Bwd.bwd =
  | Emp
  | Snoc of 'a bwd * 'a
[@@deriving show]

type backtrace = Asai.Diagnostic.backtrace Bwd.bwd
[@printer pp_bwd]

type severity = Asai.Diagnostic.severity =
  | Hint
  | Info
  | Warning
  | Error
  | Bug
[@@deriving show]

let pp_loctext = fun fmt text -> Format.pp_print_string fmt (Asai.Diagnostic.string_of_text Asai.Range.(text.value))

type 'a diagnostic =
  'a Asai.Diagnostic.t = {
  severity: Asai.Diagnostic.severity; [@printer pp_severity]
  message: 'a;
  explanation: Asai.Diagnostic.loctext;
    [@printer pp_loctext]
  backtrace: Asai.Diagnostic.backtrace; [@printer pp_bwd pp_loctext]
  extra_remarks: Asai.Diagnostic.loctext Bwd.bwd; [@printer pp_bwd pp_loctext]
}
[@@deriving show]

let message = testable Reporter.Message.pp ( = )

let code = testable Code.pp ( = )
let syn = testable Syn.pp ( = )
let path = testable Trie.pp_path ( = )
let data = testable Resolver.P.pp_data ( = )
let diagnostic =
  let pp = pp_diagnostic Reporter.Message.pp in
  testable pp ( = )

let iri = testable Iri.pp ( = )

let config = Alcotest.testable Config.pp ( = )

let document =
  let pp fmt t = Format.pp_print_string fmt (Lsp.Text_document.text t) in
  testable pp ( = )

let code = testable Code.pp ( = )
let tree = testable Code.pp_tree ( = )

let target = testable Iri_resolver.pp_target ( = )
