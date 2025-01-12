(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_core

(* We need a good solution for the mismatch between internal IRIs and URIs.
   URIs refer to the absolute path of the document, whereas IRIs don't keep
   track of the directory. Furthermore, a text document may contain changes
   that have not yet been saved to disk, so resolving the content of a URI is
   not as simple as loading the file.
   *)

type index = Uri of Lsp.Uri.t | Iri of iri

type _ target =
  | To_path : string target
  | To_uri : Lsp.Uri.t target
  | To_doc : Lsp.Text_document.t target
  | To_code : Code.tree target

let resolve
    : type a. index -> a target -> State.t -> a option
  = fun index target forest ->
    let host = forest.config.host in
    let find_uri tbl iri =
      tbl
      |> Hashtbl.to_seq_keys
      |> Seq.find (fun uri -> Iri_util.uri_to_iri ~host uri = iri)
      |> function
      | None -> None
      | Some uri ->
        Hashtbl.find_opt forest.documents uri
        |> function
        | None -> None
        | Some doc ->
          doc
          |> Lsp.Text_document.documentUri
          |> Option.some
    in
    match target with
    | To_uri ->
      begin
        match index with
        | Uri uri -> Some uri
        | Iri iri -> find_uri forest.documents iri
      end
    | To_path ->
      begin
        match index with
        | Uri uri -> Some (Lsp.Uri.to_path uri)
        | Iri iri ->
          (* We use the document store to resolve iris. It is the responsibility of
             the initializing code and the didOpen/didChange handlers to ensure that
             the hashtable remains accurate*)
          find_uri forest.documents iri |> Option.map Lsp.Uri.to_path
      end
    | To_doc ->
      begin
        match index with
        | Uri uri -> Hashtbl.find_opt forest.documents uri
        | Iri iri ->
          Option.bind
            (find_uri forest.documents iri)
            (Hashtbl.find_opt forest.documents)
      end
    | To_code ->
      begin
        match index with
        | Uri uri ->
          let iri = Iri_util.uri_to_iri ~host uri in
          Forest.find_opt forest.parsed iri
        | Iri iri -> Forest.find_opt forest.parsed iri
      end
