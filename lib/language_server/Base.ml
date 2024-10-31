(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_frontend
open Forester_compiler

module L = Lsp.Types

type server = {
  env: Forest_reader.env;
  units: Expand.Env.t;
  config: Forester_frontend.Config.Forest_config.t;
  lsp_io: LspEio.io;
  should_shutdown: bool;
  source: string option;
  (* One hashtbl per phase? Annoying...*)
  resolver: (iri, L.TextDocumentIdentifier.t) Hashtbl.t;
  documents: (L.TextDocumentIdentifier.t, Lsp.Text_document.t) Hashtbl.t;
  codes: (L.TextDocumentIdentifier.t, Code.tree) Hashtbl.t;
}

module State = Algaeff.State.Make(struct type t = server end)
