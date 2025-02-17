(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
module L = Lsp.Types

(* Reporting diagnostics requires a document URI to publish *)
let guess_uri (d : Reporter.diagnostic) =
  match d with
  | { explanation; _ } ->
    match explanation.loc with
    | None -> None
    | Some loc ->
      match Range.view loc with
      | `End_of_file { source; _ }
      | `Range ({ source; _ }, _) ->
        match source with
        | `String _ -> None
        | `File path ->
          if path <> "" then
            Some (Lsp.Uri.of_path path)
          else None

let start_of_file =
  let beginning = L.Position.create ~character: 0 ~line: 0 in
  L.Range.create ~start: beginning ~end_: beginning
