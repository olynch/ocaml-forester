open Forester_prelude
open Forester_core
open Forester_compiler

module T = Xml_tree

type env = Eio_unix.Stdenv.base

let read_trees ~(env : env) (trees : Code.tree list) : T.content T.article Addr_map.t =
  let unexpanded_trees =
    let alg acc (tree : Code.tree) =
      match tree.addr with
      | Some addr -> Addr_map.add (Addr.user_addr addr) tree acc
      | None -> acc
    in
    List.fold_left alg Addr_map.empty trees
  in
  let add_tree (tree : _ T.article) trees =
    let addr = tree.frontmatter.addr in
    if Addr_map.mem addr trees && Addr.is_user_addr addr then
      begin
        Reporter.emitf Duplicate_tree "skipping duplicate tree at address `%a`" Addr.pp addr;
        trees
      end
    else
      Addr_map.add addr tree trees
  in
  let (_, trees, jobs) =
    let task addr (units, trees, jobs) =
      let tree = Addr_map.find_opt addr unexpanded_trees in
      match tree with
      | None -> units, trees, jobs
      | Some tree ->
        let units, syn = Expand.expand_tree units tree in
        let result = Eval.eval_tree ~addr ~source_path: tree.source_path syn in
        units, List.fold_right add_tree (result.main :: result.side) trees, result.jobs @ jobs
    in
    Import_graph.topo_fold
      task
      (Import_graph.build trees)
      (Expand.Env.empty, Addr_map.empty, [])
  in
  let run_job ~env job : _ T.article =
    let@ () = Reporter.easy_run in
    match job with
    | Eval.LaTeX_to_svg{ hash; source; content } ->
      let svg = Build_latex.latex_to_svg ~env source in
      let frontmatter = { T.empty_frontmatter with addr = Addr.hash_addr hash } in
      let mainmatter = content ~svg in
      let backmatter = [] in
      T.{ frontmatter; mainmatter; backmatter }
  in
  let job_results = Eio.Fiber.List.map ~max_fibers: 20 (run_job ~env) jobs in
  List.fold_right add_tree job_results trees
