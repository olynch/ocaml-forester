(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_compiler
open Forest_graphs

module T = Types
type resource = T.content T.resource

type t = {
  env: Eio_unix.Stdenv.base;
  config: Config.Forest_config.t;
  units: Expand.Env.t;
  documents: (Lsp.Uri.t, Lsp.Text_document.t) Hashtbl.t;
  parsed: Code.tree Forest.t;
  expanded: Syn.t Forest.t;
  diagnostics: Diagnostics.table;
  resources: resource Forest.t;
  graphs: (module Forest_graphs.S);
}
