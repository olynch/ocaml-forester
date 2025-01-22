(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
module T := Types

type result = {
  articles: T.content T.article list;
  jobs: Job.job Range.located list
} [@@deriving show]

val eval_tree :
  ?quit_on_failure: bool ->
  host: string ->
  iri: iri ->
  source_path: string option ->
  Syn.tree ->
  Reporter.diagnostic list * result
