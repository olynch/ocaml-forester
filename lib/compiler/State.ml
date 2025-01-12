(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T = Types
type resource = T.content T.resource

type t = {
  env: Eio_unix.Stdenv.base;
  config: Config.t;
  units: Expand.Env.t;
  documents: (Lsp.Uri.t, Lsp.Text_document.t) Hashtbl.t;
  parsed: Code.tree Forest.t;
  expanded: Syn.t Forest.t;
  diagnostics: Diagnostic_store.t;
  resources: resource Forest.t;
  graphs: (module Forest_graphs.S);
}

let documents (t : t) = t.documents
let parsed (t : t) = t.parsed
let resources (t : t) = t.resources
let expanded (t : t) = t.expanded
let graphs (t : t) = t.graphs
let config (t : t) = t.config
let env (t : t) = t.env
let diagnostics (t : t) = t.diagnostics
let units (t : t) = t.units

let with_config
    : Config.t -> t -> t
  = fun config forest ->
    { forest with config }
