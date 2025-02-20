(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_core

module T = Types

type state = State.t

type transition = state -> state

let init ~(env : Eio_unix.Stdenv.base) ~(config : Config.t) ~(dev : bool) : state =
  Logs.debug (fun m -> m "Initializing with config %a" Config.pp config);
  let graphs = (module Forest_graphs.Make (): Forest_graphs.S) in
  let parsed = Forest.create 1000 in
  let documents = Hashtbl.create 1000 in
  let resolver = Iri_tbl.create 1000 in
  let expanded = Forest.create 1000 in
  let resources = Forest.create 1000 in
  let diagnostics = Diagnostic_store.create 100 in
  let units = Expand.Env.empty in
  {
    env;
    dev;
    config;
    units;
    documents;
    diagnostics;
    parsed;
    expanded;
    resources;
    resolver;
    graphs;
  }

let load
  : Eio.Fs.dir_ty Eio.Path.t list -> transition
= fun tree_dirs forest ->
  Logs.debug (fun m -> m "loading trees from file system");
  let paths = Dir_scanner.scan_directories tree_dirs in
  paths
  |> Seq.iter
      begin
        fun path ->
          let content = Eio.Path.load path in
          let path_str = Eio.Path.native_exn path in
          assert (not @@ Filename.is_relative path_str);
          let uri = Lsp.Uri.of_path path_str in
          let iri = Iri_scheme.uri_to_iri ~host: forest.config.host uri in
          let tree =
            Lsp.Text_document.make
              ~position_encoding: `UTF8
              {
                textDocument = {
                  languageId = "forester";
                  text = content;
                  uri;
                  version = 1
                }
              }
          in
          Iri_tbl.add forest.resolver iri path_str;
          Hashtbl.replace forest.documents uri tree
      end;
  let loaded = forest.documents |> Hashtbl.length in
  Logs.debug (fun m -> m "loaded %i trees" loaded);
  forest

let load_configured_dirs
  : transition
= fun forest ->
  let@ () = Reporter.tracef "when loading trees from disk" in
  let tree_dirs = Eio_util.paths_of_dirs ~env: forest.env forest.config.trees in
  load tree_dirs forest

let parse
  : quit_on_error: bool -> transition
= fun ~quit_on_error forest ->
  let host = forest.config.host in
  begin
    forest.documents
    |> Hashtbl.iter @@ fun uri doc ->
      let iri = Iri_scheme.uri_to_iri ~host uri in
      let source_path = Lsp.Uri.to_path uri in
      let parse_result =
        let@ () = Reporter.tracef "when parsing %a (%s)" pp_iri iri source_path in
        let@ code = Result.map @~ Parse.parse_document doc in
        Code.{
          code;
          iri = Some iri;
          source_path = Some source_path;
        }
      in
      begin
        match parse_result with
        | Error diagnostic ->
          if quit_on_error then
            begin
              Reporter.Tty.display diagnostic;
              exit 1;
            end
          else
            Diagnostic_store.replace forest.diagnostics uri [diagnostic];
        | Ok tree -> Forest.add forest.parsed iri tree
      end;
  end;
  let parsed = forest.parsed |> Forest.length in
  if not quit_on_error then
    (* If quit_on_error is true and we have encountered an error, we have
       already exited at this point. This means there should be exactly as
       many parsed trees as loaded documents*)
    assert (parsed = Hashtbl.length forest.documents);
  Logs.debug (fun m -> m "parsed %i trees" parsed);
  forest

(* FIXME: Amend import graph *)
let reparse (doc : Lsp.Text_document.t) : transition = fun forest ->
  Logs.debug (fun m -> m "reparsing");
  let host = forest.config.host in
  let uri = Lsp.Text_document.documentUri doc in
  let path = Lsp.Uri.to_path uri in
  match Parse.parse_document doc with
  | Ok code ->
    let tree =
      Code.{
        code;
        iri = Option.some @@ Iri_scheme.uri_to_iri ~host uri;
        source_path = Some path
      }
    in
    Forest.add forest.parsed (Iri_scheme.uri_to_iri ~host uri) tree;
    Eio.traceln "No parse errors. Clearing previous diagnostics";
    Diagnostic_store.remove forest.diagnostics uri;
    forest
  | Error d ->
    (* When we get a parsing diagnostic, we don't need to merge the value
       with the diagnostics from previous passes*)
    Diagnostic_store.replace forest.diagnostics uri [d];
    forest

let build_import_graph (forest : state) : state =
  (* I chose not to mention the graph in the trace message since I feel like
     it unnecessarily exposes implementation details.*)
  let@ () = Reporter.trace "when resolving imports" in
  let module Graphs = (val forest.graphs) in
  let import_graph = Imports.build forest in
  Graphs.add_graph Builtin_relation.imports import_graph;
  forest

let build_import_graph_for ~(iri : iri) (forest : state) : state =
  match Dir_scanner.find_tree (Eio_util.paths_of_dirs ~env: forest.env forest.config.trees) iri with
  | None -> Reporter.fatalf Resource_not_found "Could not find tree %a in the configured directories." pp_iri iri
  | Some source_path ->
    match Parse.parse_file source_path with
    | Ok code ->
      let tree = Code.{code; iri = Some iri; source_path = Some source_path} in
      let module Graphs = (val forest.graphs) in
      let import_graph = Imports.dependencies tree forest in
      Graphs.add_graph Builtin_relation.imports import_graph;
      forest
    | Error _ -> Reporter.fatalf Parse_error ""

let expand ~(quit_on_error : bool) (forest : state) : state =
  let parsed = forest.parsed in
  let module Graphs = (val forest.graphs) in
  let task (addr : Vertex.t) (units, trees) =
    match addr with
    | T.Content_vertex _ ->
      (* when creating the import graph we are only adding iri vertices *)
      assert false
    | T.Iri_vertex iri ->
      match Forest.find_opt parsed iri with
      | None ->
        (* The import graph has subtrees for vertices, which do not correspond to a source file *)
        units, trees
      | Some tree ->
        let diagnostics, units, syn =
          Expand.expand_tree
            ~quit_on_error
            ~host: forest.config.host
            units
            tree
        in
        let source_path = tree.source_path in
        units, (diagnostics, source_path, syn) :: trees
  in
  let units, expanded_trees =
    Forest_graph.topo_fold
      task
      (Graphs.get_rel Query.Edges Builtin_relation.imports)
      (Expand.Env.empty, [])
  in
  begin
    let@ (diagnostics, source_path, (syn : Syn.tree)) = List.iter @~ expanded_trees in
    let@ iri = Option.iter @~ syn.iri in
    Forest.replace forest.expanded iri syn;
    match source_path with
    | None ->
      Logs.warn (fun m -> m "tree at %a has no source path." pp_iri iri);
      (* There is an implicit assumption here that diagnostics are only
          emitted for trees that have a source path. I should think about
          this.*)
      if forest.dev then assert false
    | Some path ->
      assert (not (Filename.is_relative path));
      match diagnostics with
      | [] -> ()
      | diagnostics ->
        let uri = Lsp.Uri.of_path path in

        (* If we obtained some diagnostics from expanding, we have
        succesfully parsed the tree and can thus overwrite the parsing
        diagnostics *)
        Diagnostic_store.replace forest.diagnostics uri diagnostics
  end;
  let expanded = forest.expanded |> Forest.length in
  let diagnostics = forest.diagnostics |> Diagnostic_store.length in
  Logs.debug (fun m -> m "expanded %i trees" expanded);
  Logs.debug (fun m -> m "%i trees emitted diagnostics." diagnostics);
  State.with_units units forest

(* There is some duplicated code here. The only significant difference is the
   fact that we are using a different graph builder, one that only traverses
   the dependencies of a specific tree*)
let expand_only_aux ~(quit_on_error : bool) ~(addr : iri) (forest : state) : Expand.Env.t * Diagnostic_store.t * Syn.tree Forest.t =
  let import_graph =
    Imports.run_builder
      ~root: addr
      {
        graph = Forest_graph.create ();
        follow = true;
        forest;
      }
  in
  assert (Forest_graph.nb_vertex import_graph >= Forest.length forest.parsed);
  let task (addr : Vertex.t) (units, diagnostics, trees) =
    match addr with
    | T.Content_vertex _ ->
      (* when creating the import graph we are only adding iri vertices *)
      assert false
    | T.Iri_vertex iri ->
      match Imports.resolve_iri_to_code iri forest with
      | None ->
        (* The import graph has vertices for subtrees, which do not correspond to a source file *)
        Logs.debug (fun m -> m "failed to resolve %a" pp_iri iri);
        units, diagnostics, trees
      | Some tree ->
        let ds, units, syn = Expand.expand_tree ~quit_on_error ~host: forest.config.host units tree in
        let source_path = tree.source_path in
        begin
          Forest.add trees iri syn;
          match source_path with
          | None ->
            Logs.warn (fun m -> m "Could not construct URI for tree at %a. There may be missing diagnostics." pp_iri iri)
          | Some path ->
            let uri = Lsp.Uri.of_path path in
            match ds with
            | [] -> ()
            | ds -> Diagnostic_store.add diagnostics uri ds
        end;
        units, diagnostics, trees
  in
  let units, diagnostics, expanded_trees =
    Logs.debug (fun m -> m "Folding the import graph with %i vertices" (Forest_graph.nb_vertex import_graph));
    Forest_graph.topo_fold
      task
      import_graph
      (Expand.Env.empty, Diagnostic_store.create 100, Forest.create 1000)
  in
  Logs.debug (fun m -> m "Got %i expanded trees" (Forest.length expanded_trees));
  units, diagnostics, expanded_trees

(* The purpose of this function is to update the exported units when a tree has
   been changed when running with the lsp.*)
let expand_only (iri : iri) : transition = fun forest ->
  let units, new_diagnostics, trees =
    expand_only_aux
      ~quit_on_error: false
      ~addr: iri
      forest
  in
  assert (Forest.length trees > 0);
  begin
    trees
    |> Forest.iter @@ fun iri tree ->
      Forest.replace forest.expanded iri tree;
      match Iri_tbl.find_opt forest.resolver iri with
      | None ->
        Logs.debug (fun m -> m "resolver knows about %i paths" (Iri_tbl.length forest.resolver));
        assert false
      | Some path ->
        Logs.debug (fun m -> m "clearing diagnostics for %s" path);
        Diagnostic_store.remove forest.diagnostics (Lsp.Uri.of_path path)
  end;
  begin
    new_diagnostics
    |> Diagnostic_store.iter @@ fun uri diagnostics ->
      Logs.debug (fun m -> m "%s got some expansion diagnostics." (Lsp.Uri.to_string uri));
      match diagnostics with
      | [] -> ()
      | diagnostics ->
        Diagnostic_store.append forest.diagnostics uri diagnostics
  end;
  (*FIXME: Don't replace all units. Just update the ones that have changed!*)
  {forest with units}

let export_publication ~env ~(forest : state) (publication : Job.publication) : unit =
  let vertices = Forest.run_datalog_query forest.graphs publication.query in
  let resources =
    let@ vertex = List.filter_map @~ Vertex_set.elements vertices in
    match vertex with
    | Content_vertex _ -> None
    | Iri_vertex iri ->
      Forest.find_opt forest.resources iri
  in
  match publication.format with
  | Json_blob ->
    let blob = Repr.to_json_string ~minify: true (T.forest_t T.content_t) resources in
    let cwd = Eio.Stdenv.cwd env in
    let dir = Eio.Path.(cwd / "publications") in
    let filename = publication.name ^ ".json" in
    let path = Eio.Path.(dir / filename) in
    Eio.Path.mkdirs ~exists_ok: true ~perm: 0o755 dir;
    Eio.Path.save ~create: (`Or_truncate 0o644) path blob;
    assert (Eio.Path.is_file path)

let eval_tree (forest : state) iri syn =
  let host = forest.config.host in
  let env = forest.env in
  let@ () = Reporter.tracef "when evaluating %a" pp_iri iri in
  let source_path = if forest.dev then Iri_tbl.find_opt forest.resolver iri else None in
  let uri = Option.map Lsp.Uri.of_path source_path in
  let diagnostics, Eval.{articles; jobs} =
    Eval.eval_tree
      ~quit_on_failure: false
      ~host
      ~source_path
      ~iri
      syn
  in
  let append_diagnostics () =
    let@ uri = Option.iter @~ uri in
    Diagnostic_store.append forest.diagnostics uri diagnostics
  in
  let plant_articles () =
    let@ article = List.iter @~ articles in
    Forest.plant_resource (T.Article article) forest.graphs forest.resources
  in
  append_diagnostics ();
  plant_articles ();
  begin
    let@ Range.{value; loc} = Eio.Fiber.List.iter ~max_fibers: 20 @~ jobs in
    let@ () = Reporter.easy_run in
    match value with
    | Job.LaTeX_to_svg {hash; source; content} ->
      let svg = Build_latex.latex_to_svg ~env ?loc source in
      let iri = Iri_scheme.hash_iri ~host hash in
      let frontmatter = T.default_frontmatter ~iri () in
      let mainmatter = content ~svg in
      let backmatter = T.Content [] in
      let article = T.{frontmatter; mainmatter; backmatter} in
      Forest.plant_resource (T.Article article) forest.graphs forest.resources
    | Job.Publish publication ->
      export_publication ~env ~forest publication
  end

let eval : transition = fun forest ->
  let expanded = forest.expanded in
  expanded |> Forest.iter (eval_tree forest);
  Logs.debug (fun m -> m "evaluated %i resources" (Forest.length forest.resources));
  forest

let eval_only
  (iri : iri)
  : transition
= fun forest ->
  match Forest.find_opt forest.expanded iri with
  | None -> assert false
  | Some syn ->
    (* NOTE: Not running jobs. *)
    let diagnostics, Eval.{articles; jobs = _} =
      Eval.eval_tree
        ~quit_on_failure: false
        ~host: forest.config.host
        ~source_path: None
        ~iri
        syn
    in
    begin
      let@ article = List.iter @~ articles in
      Forest.plant_resource (Article article) forest.graphs forest.resources
    end;
    begin
      let@ uri =
        Option.iter @~
        Option.map Lsp.Uri.of_path @@
        Iri_tbl.find_opt forest.resolver iri
      in
      Diagnostic_store.append
        forest.diagnostics
        uri
        diagnostics;
    end;
    forest

let implant_foreign
  : transition
= fun state ->
  begin
    let foreign_paths = Eio_util.paths_of_files ~env: state.env state.config.foreign in
    let module EP = Eio.Path in
    let@ path = List.iter @~ foreign_paths in
    let path_str = EP.native_exn path in
    Reporter.log Format.pp_print_string (Format.sprintf "Implant foreign forest from `%s'" path_str);
    let blob = try EP.load path with _ -> Reporter.fatalf IO_error "Could not read foreign forest blob at `%s`" path_str in
    match Repr.of_json_string (T.forest_t T.content_t) blob with
    | Ok foreign_forest ->
      List.iter (fun r -> Forest.plant_resource r state.graphs state.resources) foreign_forest
    | Error (`Msg err) ->
      Reporter.fatalf Parse_error "Could not parse foreign forest blob: %s" err
    | exception (Iri.Error err) ->
      Reporter.fatalf Parse_error "Encountered error while decoding foreign forest blob: %s" (Iri.string_of_error err)
    | exception exn ->
      Reporter.fatalf Parse_error "Encountered unknown error while decoding foreign forest blob: %s" (Printexc.to_string exn)
  end;
  state

let plant_assets
  : transition
= fun state ->
  let@ () = Reporter.tracef "when planting assets" in
  let paths =
    Dir_scanner.scan_asset_directories
      (Eio_util.paths_of_dirs ~env: state.env state.config.assets)
  in
  Logs.debug (fun m -> m "got %i asset paths" (Seq.length paths));
  Logs.debug (fun m -> m "planting %i assets" (Seq.length paths));
  let module EP = Eio.Path in
  begin
    let@ path = Eio.Fiber.List.iter @~ List.of_seq paths in
    let content = EP.load path in
    let source_path = EP.native_exn path in
    let iri = Asset_router.install ~host: state.config.host ~source_path ~content in
    Logs.debug (fun m -> m "Installed %s at %a" source_path pp_iri iri);
    Forest.plant_resource (T.Asset {iri; host = state.config.host; content}) state.graphs state.resources;
  end;
  state
