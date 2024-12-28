(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

module T = Forester_core.Types

type resource = T.content T.resource

type t = {
  env: Eio_unix.Stdenv.base;
  config: Config.Forest_config.t;
  units: Forester_compiler.Expand.Env.t;
  documents: (Lsp.Uri.t, Lsp.Text_document.t) Hashtbl.t;
  parsed: Forester_compiler.Code.tree Forest.t;
  expanded: Forester_compiler.Syn.t Forest.t;
  diagnostics: Diagnostics.table;
  resources: resource Forest.t;
  graphs: (module Forest_graphs.S);
}
