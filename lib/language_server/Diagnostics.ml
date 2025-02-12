(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

(* When computing the diagnostics for a specific text document, do we know that
   the emitted diagnostics should be reported to the same URI?
   *)

open Forester_compiler

module L = Lsp.Types

let compute (document : Lsp.Text_document.t) =
  let Lsp_state.{ forest; _ } = Lsp_state.get () in
  let config = State.config forest in
  let uri = Lsp.Text_document.documentUri document in
  let iri = Iri_util.uri_to_iri ~host: config.host uri in
  Phases.(
    forest
    |> reparse document
    |> expand_only iri
    |> eval
    |> State.diagnostics
    |> Diagnostic_store.iter
      (
        fun uri diagnostics ->
          match diagnostics with
          | [] ->
            Eio.traceln "Clearing diagnostics for %s" (Lsp.Uri.to_path uri);
            Publish.publish uri []
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
