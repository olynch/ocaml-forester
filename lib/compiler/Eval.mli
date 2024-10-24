(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
module T := Types

type result = {
  main: T.content T.article;
  side: T.content T.article list;
  jobs: Job.job list
}

val eval_tree : host: string -> iri: iri -> source_path: string option -> Syn.tree -> result
