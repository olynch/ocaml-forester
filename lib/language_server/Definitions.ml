(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core

module L = Lsp.Types

let (let*) = Option.bind

let compute
    (params : L.DefinitionParams.t)
    : L.Locations.t option
  =
  match params with
  | {
    position;
    textDocument;
    _;
  } ->
    let server = State.get () in
    let codes = server.index.codes in
    let* { code; _ } = Hashtbl.find_opt codes { uri = textDocument.uri } in
    let* addr = Analysis.addr_at ~position code in
    let iri = Iri_scheme.user_iri ~host: server.config.host addr in
    let* uri = Hashtbl.find_opt server.index.resolver iri in
    let range = L.Range.create ~start: { character = 1; line = 0 } ~end_: { character = 1; line = 0 } in
    Some
      (`Location [L.Location.{ uri = uri.uri; range }])
