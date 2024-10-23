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

let read_trees ~(env : env) ~host (trees : Code.tree list) : T.content T.article Iri_map.t =
  let unexpanded_trees = organise_trees ~host trees in
  let add_article (article : _ T.article) articles =
    match article.frontmatter.iri with
    | Some iri -> Iri_map.add iri article articles
    | None -> articles
  in
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
      (Expand.Env.empty, Iri_map.empty, [])
  in
  let run_job ~env job : _ T.article =
    let@ () = Reporter.easy_run in
    match job with
    | Eval.LaTeX_to_svg { hash; source; content } ->
      let svg = Build_latex.latex_to_svg ~env source in
      let frontmatter = T.default_frontmatter ~iri: (Iri_scheme.hash_iri ~host hash) () in
      let mainmatter = content ~svg in
      let backmatter = T.Content [] in
      T.{ frontmatter; mainmatter; backmatter }
  in
  let job_results = Eio.Fiber.List.map ~max_fibers: 20 (run_job ~env) jobs in
  List.fold_right add_article job_results articles
