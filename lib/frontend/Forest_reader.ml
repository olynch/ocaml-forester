(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

module T = Types

type env = Eio_unix.Stdenv.base

let organise_trees ~host (trees : Code.tree list) : Code.tree String_map.t =
  let base = Iri_scheme.base_iri ~host in
  let alg acc (tree : Code.tree) =
    match tree.addr with
    | Some addr -> String_map.add addr tree acc
    | None -> acc
  in
  List.fold_left alg String_map.empty trees

module Job_runner = struct
  type env = { env: Eio_unix.Stdenv.base; host: string }
  type input = Job.job
  type output = { articles: T.content T.article list; publications: Job.publication list }

  let nil = { articles = []; publications = [] }
  let plus o1 o2 = { articles = o1.articles @ o2.articles; publications = o1.publications @ o2.publications }

  let eval { env; host } job =
    let@ () = Reporter.easy_run in
    match job with
    | Job.LaTeX_to_svg { hash; source; content } ->
      let svg = Build_latex.latex_to_svg ~env source in
      let frontmatter = T.default_frontmatter ~iri: (Iri_scheme.hash_iri ~host hash) () in
      let mainmatter = content ~svg in
      let backmatter = T.Content [] in
      let article = T.{ frontmatter; mainmatter; backmatter } in
      { nil with articles = [article] }
    | Job.Publish publication ->
      { nil with publications = [publication] }
end

module Eval_runner = struct
  type env = { host: string }
  type input = string * string option * Syn.t
  type output = { articles: T.content T.article list; jobs: Job.job list }

  let nil = { articles = []; jobs = [] }
  let plus o1 o2 = { articles = o1.articles @ o2.articles; jobs = o1.jobs @ o2.jobs }

  let eval { host } (addr, source_path, syn) =
    let@ () = Reporter.easy_run in
    let iri = Iri_scheme.user_iri ~host addr in
    let result = Eval.eval_tree ~host ~iri ~source_path syn in
    { articles = result.main :: result.side; jobs = result.jobs }
end

module Job_map_reduce = Map_reduce.Make(Job_runner)
module Eval_map_reduce = Map_reduce.Make(Eval_runner)

let expand ~host trees =
  let unexpanded_trees = organise_trees ~host trees in
  let task addr (units, trees) =
    match String_map.find_opt addr unexpanded_trees with
    | None -> units, trees
    | Some tree ->
      let units, syn = Expand.expand_tree units tree in
      units, (addr, tree.source_path, syn) :: trees
  in
  Import_graph.topo_fold task (Import_graph.build trees) (Expand.Env.empty, [])

let read_trees ~(env : env) ~host (trees : Code.tree list) : T.content T.article list * Job.publication list =
  let _, expanded_trees = expand ~host trees in
  let Eval_runner.{ articles; jobs } = Eval_map_reduce.reduce ~env: { host } expanded_trees in
  let job_results =
    Job_map_reduce.reduce
      ~init: { Job_runner.nil with articles }
      ~env: { env; host }
      jobs
  in
  job_results.articles, job_results.publications
