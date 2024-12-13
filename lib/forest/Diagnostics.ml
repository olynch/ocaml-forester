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

type table = diagnostics t

let append
    : diagnostics t -> key -> diagnostics -> unit
  = fun table uri diagnostics ->
    match find_opt table uri with
    | None ->
      add table uri diagnostics
    | Some previous ->
      replace table uri (previous @ diagnostics)
