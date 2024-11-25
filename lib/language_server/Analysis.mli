(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core

module G: Forester_forest.Forest_graphs.S
module F: Forester_forest.Forest.S
module L = Lsp.Types
module T = Forester_core.Types
module EP = Eio.Path

type diagnostic = Reporter.Message.t Asai.Diagnostic.t

module PT:
sig
  val string_of_content : T.content -> string
  val pp_content : Format.formatter -> T.content -> unit
end

val update_graph : Lsp.Uri.t -> Forester_compiler.Code.t -> unit

val parse_from :
  [< `Eio_path of [> Eio.Fs.dir_ty] EP.t
  | `Iri of < fs: [> Eio.Fs.dir_ty] EP.t; .. > * iri
  | `Text_document of Lsp.Text_document.t
  | `String of string
  | `Uri of L.TextDocumentIdentifier.t] ->
  (Forester_compiler.Code.t, Reporter.Message.t Asai.Diagnostic.t) result

val syntax_diagnostics :
  [< `Eio_path of [> Eio.Fs.dir_ty] EP.t
  | `Iri of < fs: [> Eio.Fs.dir_ty] EP.t; .. > * iri
  | `Text_document of Lsp.Text_document.t
  | `String of string
  | `Uri of L.TextDocumentIdentifier.t] ->
  Reporter.Message.t Asai.Diagnostic.t list

val expand :
  unit ->
  (L.DocumentUri.t, diagnostic) Hashtbl.t
  * Forester_compiler.Expand.Env.t
  * (string * string option * Forester_compiler__.Syn.t) list

val check_semantics : L.DocumentUri.t -> unit

val extract_addr :
  Forester_compiler.Code.node Forester_core.Range.located ->
  string option

val node_at :
  position: Lsp.Types.Position.t ->
  Forester_compiler.Code.t ->
  Forester_compiler.Code.node Range.located option

val addr_at :
  position: Lsp.Types.Position.t ->
  Forester_compiler.Code.t ->
  string option
