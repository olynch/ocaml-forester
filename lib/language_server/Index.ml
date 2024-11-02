(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler

module L = Lsp.Types

type resolver = (iri, L.TextDocumentIdentifier.t) Hashtbl.t

type t = {
  resolver: resolver;
  documents: (L.TextDocumentIdentifier.t, Lsp.Text_document.t) Hashtbl.t;
  codes: (L.TextDocumentIdentifier.t, Code.tree) Hashtbl.t;
}
