open Forester_core

module T = Xml_tree

type 'a env = 'a constraint 'a = <
    cwd : Eio.Fs.dir_ty Eio.Path.t;
    process_mgr : _ Eio.Process.mgr;
    stdout : _ Eio.Flow.sink;
    ..
  > as 'a

exception Todo

let read_trees ~(env : _ env) (trees : Code.tree list) : T.content T.article Addr_map.t =
  let unexpanded_trees =
    let alg acc (tree : Code.tree) =
      match tree.addr with
      | Some addr -> Addr_map.add (User_addr addr) tree acc
      | None -> acc
    in
    List.fold_left alg Addr_map.empty trees
  in

  let add_tree (tree : _ T.article) trees =
    let addr = tree.frontmatter.addr in
    if Addr_map.mem addr trees && is_user_addr addr then
      begin
        Reporter.emitf Duplicate_tree "skipping duplicate tree at address `%a`" pp_addr addr;
        trees
      end
    else
      Addr_map.add addr tree trees
  in

  let (_, trees, jobs) =
    let import_graph = Import_graph.build_import_graph trees in
    let task addr (units, trees, jobs) =
      let tree = Addr_map.find_opt addr unexpanded_trees in
      match tree with
      | None -> units, trees, jobs
      | Some tree ->
        let units, syn = Expand.expand_tree units tree in
        let result = Eval.eval_tree ~addr ~source_path:tree.source_path syn in
        let add trees (emitted : _ T.article)  =
          add_tree emitted trees
        in
        units, List.fold_right add_tree (result.main :: result.side) trees,   result.jobs @ jobs
    in
    Import_graph.Topo.fold task import_graph (Expand.UnitMap.empty, Addr_map.empty, [])
  in

  let run_job ~env job : _ T.article =
    Reporter.easy_run @@ fun () ->
    match job with
    | Eval.LaTeX_to_svg {hash; source; content} ->
      let svg = Forester_render.Build_latex.latex_to_svg ~env source in
      let frontmatter = {T.empty_frontmatter with addr = Hash_addr hash} in
      let mainmatter = content ~svg in
      let backmatter = [] in
      T.{frontmatter; mainmatter; backmatter}
  in

  let job_results = Eio.Fiber.List.map ~max_fibers:20 (run_job ~env) jobs in
  List.fold_right add_tree job_results trees
