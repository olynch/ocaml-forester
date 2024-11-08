(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
module L = Lsp.Types

(* Reporting diagnostics requires a document URI to publish *)
let guess_uri (d : Reporter.Message.t Asai.Diagnostic.t) =
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
          Eio.traceln "source has path %s" path;
          Some (Lsp.Uri.of_path path)

let path_to_iri ~host str =
  str
  |> String.split_on_char '/'
  |> List.rev
  |> List.hd
  |> Filename.chop_extension
  |> Iri_scheme.user_iri ~host

let uri_to_addr path =
  path
  |> String.split_on_char '/'
  |> List.rev
  |> List.hd
  |> Filename.chop_extension

let start_of_file =
  let beginning = L.Position.create ~character: 0 ~line: 0 in
  L.Range.create ~start: beginning ~end_: beginning

let lsp_report uri k =
  let diagnostics = Hashtbl.create 5 in
  let push d =
    match guess_uri d with
    | Some guessed_uri ->
      Eio.traceln "guessed %s" (Lsp.Uri.to_string guessed_uri);
      begin
        match Hashtbl.find_opt diagnostics uri with
        | Some diags -> Hashtbl.add diagnostics guessed_uri (d :: diags)
        | None -> Hashtbl.add diagnostics guessed_uri [d]
      end
    | None -> Hashtbl.add diagnostics uri [d]
  in
  (
    Reporter.run ~emit: push ~fatal: push @@
      fun () ->
        begin
          k ()
        end
  );
  Hashtbl.iter
    (
      fun uri ds ->
        Eio.traceln "publishing %i diagnostics to %s" (List.length ds) (Lsp.Uri.to_string uri);
        Publish.to_uri uri ds
    )
    diagnostics
