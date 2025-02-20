(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

(* open Forester_core *)
(* open Forester_frontend *)

module L = Lsp.Types

let (let*) = Option.bind

let compute
    (params : L.DefinitionParams.t)
    : L.Locations.t option
  =
  match params with
  | {textDocument;
    _;
  } ->
    (* let server = State.get () in *)
    (* let host = server.config.host in *)
    (* let codes = server.parsed in *)
    (* let resolver = Compiler.make_resolver ~host: server.config.host codes in *)
    (* let* { code; _ } = Compiler.resolve ~host textDocument.uri codes in *)
    (* let* addr = Analysis.addr_at ~position code in *)
    (* let iri = Iri_scheme.user_iri ~host: server.config.host addr in *)
    (* let* uri = Hashtbl.find_opt resolver iri in *)
    let range = L.Range.create ~start: {character = 1; line = 0} ~end_: {character = 1; line = 0} in
    Some
      (`Location [L.Location.{uri = textDocument.uri; range}])
