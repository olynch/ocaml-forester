(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

(* When computing the diagnostics for a specific text document, do we know that
   the emitted diagnostics should be reported to the same URI?
   *)

open Forester_forest
open Forester_frontend

module L = Lsp.Types

let compute (document : Lsp.Text_document.t) =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let config = Compiler.get_config forest in
  let uri = Lsp.Text_document.documentUri document in
  let iri = uri |> Iri_util.uri_to_iri ~host: config.host in
  Compiler.(
    forest
    |> reparse document
    |> expand_only iri
    |> eval ~dev: true
    |> get_diagnostics
    |> Diagnostics.iter
      (
        fun uri diagnostics ->
          match diagnostics with
          | [] -> Publish.publish uri []
          | diagnostics ->
            Eio.traceln "publishing %i diagnostics to %s" (List.length diagnostics) (Lsp.Uri.to_path uri);
            List.iter
              (
                fun d ->
                  Eio.traceln
                    "%s"
                    (
                      Asai.Diagnostic.(d.explanation.value)
                      |> Asai.Diagnostic.string_of_text
                    )
              )
              diagnostics;
            Publish.publish uri diagnostics
      )
  )
