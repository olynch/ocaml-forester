(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_core

(*  NOTE: Depending on the mode of compilation, we might encouter IRIs
    corresponding to trees that have not been loaded yet. For example, we might
    encounter \import{foo-0001}, which corresponds to a tree with source path
    ./trees/foo-0001.tree. Thus we have to check the configured directories one
    by one until we find a match.
*)

open Forester_prelude
open Forester_core

type index = Uri of Lsp.Uri.t | Iri of iri

type _ target =
  | To_uri : Lsp.Uri.t target
  | To_path : string target
  | To_doc : Lsp.Text_document.t target
  | To_code : Code.tree target
[@@deriving show]

let show_target
    : type a. a target -> string
  = function
  | To_uri -> "uri"
  | To_path -> "path"
  | To_doc -> "doc"
  | To_code -> "code"

let is_loaded iri forest = Hashtbl.mem forest iri

let delete_leading_slash path_str =
  String.sub path_str 1 ((String.length path_str) - 1)

let rec resolve_iri
    : type a. iri -> a target -> State.t -> a option
  = fun iri target forest ->
    Logs.debug (fun m -> m "resolving %a to %s" pp_iri iri (show_target target));
    match target with
    | To_code ->
      begin
        match Forest.find_opt forest.parsed iri with
        | Some code -> Some code
        | None ->
          match resolve_iri iri To_doc forest with
          | Some doc ->
            let@ code = Option.map @~ Result.to_option @@ Parse.parse_document doc in
            let source_path = Lsp.Uri.to_path (Lsp.Text_document.documentUri doc) in
            let addr = Iri_util.iri_to_addr iri in
            Code.{ code; source_path = Some source_path; addr = Some addr }
          | None ->
            Logs.debug (fun m -> m "Did not find document at %a. Trying to load document from FS" pp_iri iri);
            let@ uri = Option.bind @@ resolve_iri iri To_uri forest in
            resolve_uri uri To_code forest
      end
    | To_doc ->
      let@ uri = Option.bind @@ resolve_iri iri To_uri forest in
      resolve_uri uri To_doc forest
    | To_uri ->
      let paths = Eio_util.paths_of_dirs ~env: forest.env forest.config.trees in
      let@ path = Option.bind @@ Dir_scanner.find_tree paths (Iri_util.iri_to_addr iri) in
      Some (Lsp.Uri.of_string path)
    | To_path -> resolve_iri iri To_uri forest |> Option.map Lsp.Uri.to_path

and resolve_uri
    : type a. Lsp.Uri.t -> a target -> State.t -> a option
  = fun uri target forest ->
    Logs.debug (fun m -> m "resolving %s to %s" (Lsp.Uri.to_string uri) (show_target target));
    match target with
    | To_uri -> Some uri
    | To_path ->
      Some (Lsp.Uri.to_path uri)
    | To_doc ->
      begin
        match Hashtbl.find_opt forest.documents uri with
        | Some doc -> Some doc
        | None ->
          let path = Eio.Path.(forest.env#fs / delete_leading_slash (Lsp.Uri.to_path uri)) in
          assert (Eio.Path.is_file path);
          let content = Eio.Path.load path in
          let textDocument =
            Lsp.Types.TextDocumentItem.{
              languageId = "forester";
              text = content;
              uri;
              version = 1
            }
          in
          Option.some @@ Lsp.Text_document.make ~position_encoding: `UTF8 { textDocument }
      end
    | To_code ->
      resolve_iri
        (Iri_util.uri_to_iri ~host: forest.config.host uri)
        To_code
        forest

let resolve
    : type a. index -> a target -> State.t -> a option
  = fun index target forest ->
    let iri =
      match index with
      | Iri iri -> iri
      | Uri uri -> Iri_util.uri_to_iri ~host: forest.config.host uri
    in
    resolve_iri iri target forest
