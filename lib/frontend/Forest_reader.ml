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

let read_trees ~(env : env) ~host (trees : Code.tree list) : T.content T.article list * Job.publication list =
  let unexpanded_trees = organise_trees ~host trees in
  let add_article (article : _ T.article) articles = article :: articles in
  let (_, articles, jobs) =
    let task addr (units, trees, jobs) =
      match String_map.find_opt addr unexpanded_trees with
      | None -> units, trees, jobs
      | Some tree ->
        let units, syn = Expand.expand_tree units tree in
        let iri = Iri_scheme.user_iri ~host addr in
        let result = Eval.eval_tree ~host ~iri ~source_path: tree.source_path syn in
        units, List.fold_right add_article (result.main :: result.side) trees, result.jobs @ jobs
    in
    Import_graph.topo_fold
      task
      (Import_graph.build trees)
      (Expand.Env.empty, [], [])
  in
  let job_worker ~env job : _ =
    let@ () = Reporter.easy_run in
    match job with
    | Job.LaTeX_to_svg { hash; source; content } ->
      let svg = Build_latex.latex_to_svg ~env source in
      let frontmatter = T.default_frontmatter ~iri: (Iri_scheme.hash_iri ~host hash) () in
      let mainmatter = content ~svg in
      let backmatter = T.Content [] in
      `Article T.{ frontmatter; mainmatter; backmatter }
    | Job.Publish publication ->
      `Publication publication
  in
  let results = Eio.Fiber.List.map ~max_fibers: 20 (job_worker ~env) jobs in
  let accumulator (articles, publications) = function
    | `Article article -> article :: articles, publications
    | `Publication publication -> articles, publication :: publications
  in
  List.fold_left accumulator (articles, []) results
