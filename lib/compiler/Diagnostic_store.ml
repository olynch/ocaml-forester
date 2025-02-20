(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_core

type diagnostics = Reporter.Message.t Asai.Diagnostic.t list
module Table = Hashtbl.Make(Lsp.Uri)
include Table

type t = diagnostics Table.t

let replace
  : 'a Table.t -> key -> 'a -> unit
= fun table uri a ->
  assert (not @@ Filename.is_relative (Lsp.Uri.to_path uri));
  Table.replace table uri a

let add
  : 'a Table.t -> key -> 'a -> unit
= fun table uri a ->
  assert (not @@ Filename.is_relative (Lsp.Uri.to_path uri));
  Table.add table uri a

let append
  : t -> key -> diagnostics -> unit
= fun table uri diagnostics ->
  assert (not @@ Filename.is_relative (Lsp.Uri.to_path uri));
  match find_opt table uri with
  | None ->
    add table uri diagnostics
  | Some previous ->
    replace table uri (previous @ diagnostics)
