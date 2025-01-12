(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_core

module T = Types
module EP = Eio.Path

type state = State.t

type transition = state -> state

let init
    : env: Eio_unix.Stdenv.base -> config: Config.t -> state
  = fun ~env ~config ->
    Logs.debug (fun m -> m "Initializing with config @.%a" Config.pp config);
    let graphs = (module Forest_graphs.Make (): Forest_graphs.S) in
    let parsed = Forest.create 1000 in
    let documents = Hashtbl.create 1000 in
    let expanded = Forest.create 1000 in
    let resources = Forest.create 1000 in
    let diagnostics = Diagnostic_store.create 100 in
    let units = Expand.Env.empty in
    {
      env;
      config;
      units;
      documents;
      diagnostics;
      parsed;
      expanded;
      resources;
      graphs
    }

(*  TODO: after my refactors, it is no longer clear when and how this should be
    used. I think we need to find a definitive design for when to call
    register_iri, and no longer use a name such as plant_resource. It seems to
    me that this function conflates adding stuff to the hash table and dealing
    with the graphs.*)

let load
    : Eio.Fs.dir_ty EP.t list -> transition
  = fun tree_dirs forest ->
    Logs.debug (fun m -> m "loading trees from file system");
    tree_dirs
    |> Forest_scanner.scan_directories
    |> Seq.iter
      (
        fun path ->
          (* WARNING: using realpath. This is necessary because later on,
             the client will send notifications using the absolute path of
             a file.*)
          let source_path = EP.native_exn path |> Unix.realpath in
          let content = EP.load path in
          let uri = source_path |> Lsp.Uri.of_path in
          let tree =
            Lsp.Text_document.make
              ~position_encoding: `UTF8
              {
                textDocument =
                {
                  languageId = "forester";
                  text = content;
                  uri;
                  version = 1
                }
              }
          in
          Hashtbl.replace forest.documents uri tree
      );
    let loaded = forest.documents |> Hashtbl.length in
    Logs.debug (fun m -> m "loaded %i trees" loaded);
    forest

let load_configured_dirs
    : transition
  = fun forest ->
    let tree_dirs = Eio_util.paths_of_dirs ~env: forest.env forest.config.trees in
    load tree_dirs forest

let parse
    : quit_on_error: bool -> transition
  = fun ~quit_on_error forest ->
    Logs.debug (fun m -> m "parsing trees");
    let host = forest.config.host in
    forest.documents
    |> Hashtbl.iter
      (
        fun uri doc ->
          let iri = Iri_util.uri_to_iri ~host uri in
          let source_path = Lsp.Uri.to_path uri in
          let parse_result =
            Parse.parse_document doc
            |> Result.map
              (
                fun code ->
                  Code.{
                    code;
                    addr = Option.some (uri |> Iri_util.uri_to_addr);
                    source_path = Option.some source_path;
                  }
              )
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
      );
    let parsed = forest.parsed |> Forest.length in
    Logs.debug (fun m -> m "parsed %i trees" parsed);
    forest

(* TODO: Amend import graph *)
let reparse
    : Lsp.Text_document.t -> transition
  = fun doc forest ->
    Logs.debug (fun m -> m "reparsing");
    let host = forest.config.host in
    let uri = Lsp.Text_document.documentUri doc in
    let path = Lsp.Uri.to_path uri in
    match Parse.parse_document doc with
    | Ok code ->
      let tree =
        Code.{
          code;
          addr = Option.some (uri |> Iri_util.uri_to_addr);
          source_path = Some path
        }
      in
      Forest.add forest.parsed (Iri_util.uri_to_iri ~host uri) tree;
      Eio.traceln "No parse errors. clearing previous diagnostics";
      Diagnostic_store.remove forest.diagnostics uri;
      forest
    | Error d ->
      (* When we get a parsing diagnostic, we don't need to merge the value with the diagnostics from previous passes*)
      Diagnostic_store.replace forest.diagnostics uri [d];
      forest

let build_import_graph
    : state -> state
  = fun forest ->
    let module Graphs = (val forest.graphs) in
    let import_graph = Imports.build forest in
    Graphs.add_graph Builtin_relation.imports import_graph;
    forest

let build_import_graph_for
    : addr: iri -> state -> state
  = fun ~addr forest ->
    match Iri_resolver.(resolve (Iri addr) To_code forest) with
    | None -> forest
    | Some tree ->
      let module Graphs = (val forest.graphs) in
      let import_graph = Imports.dependencies tree forest in
      Graphs.add_graph Builtin_relation.imports import_graph;
      forest

let expand
    : quit_on_error: bool -> state -> state
  = fun ~quit_on_error forest ->
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
              units
              tree
          in
          let source_path = tree.source_path in
          units, (diagnostics, source_path, syn) :: trees
    in
    let _units, expanded_trees =
      Forest_graph.topo_fold
        task
        (Graphs.get_rel Query.Edges Builtin_relation.imports)
        (Expand.Env.empty, [])
    in
    expanded_trees
    |> List.iter
      (
        fun (diagnostics, source_path, (syn : Syn.tree)) ->
          let@ iri = Option.iter @~ Option.map (Iri_util.path_to_iri ~host: forest.config.host) source_path in
          Forest.replace forest.expanded iri syn;
          match source_path with
          | None ->
            Logs.warn (fun m -> m "tree at %a has no source path." pp_iri iri)
          | Some path ->
            match diagnostics with
            | [] -> ()
            | diagnostics ->
              let uri = Lsp.Uri.of_path path in
              Diagnostic_store.append forest.diagnostics uri diagnostics
      );
    let expanded = forest.expanded |> Forest.length in
    let diagnostics = forest.diagnostics |> Diagnostic_store.length in
    Logs.debug (fun m -> m "expanded %i trees" expanded);
    Logs.debug (fun m -> m "%i trees emitted diagnostics." diagnostics);
    forest

let expand_only_aux
    : quit_on_error: bool ->
    addr: iri ->
    State.t ->
    Expand.Env.t * Diagnostic_store.t * Syn.tree Forest.t
  = fun ~quit_on_error ~addr forest ->
    let import_graph =
      Imports.run_builder
        ~root: addr
        {
          graph = Forest_graph.create ();
          follow = true;
          forest;
        }
    in
    let task (addr : Vertex.t) (units, diagnostics, trees) =
      match addr with
      | T.Content_vertex _ ->
        (* when creating the import graph we are only adding iri vertices *)
        assert false
      | T.Iri_vertex iri ->
        match Iri_resolver.(resolve (Iri iri) To_code forest) with
        | None ->
          (* The import graph has vertices for subtrees, which do not correspond to a source file *)
          Logs.debug (fun m -> m "failed to resolve %a" pp_iri iri);
          units, diagnostics, trees
        | Some tree ->
          let ds, units, syn = Expand.expand_tree ~quit_on_error units tree in
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
              | ds ->
                Diagnostic_store.add diagnostics uri ds
          end;
          units, diagnostics, trees
    in
    let units, diagnostics, expanded_trees =
      Eio.traceln "Folding the import graph with %i vertices" (Forest_graph.nb_vertex import_graph);
      Forest_graph.topo_fold
        task
        import_graph
        (Expand.Env.empty, Diagnostic_store.create 100, Forest.create 1000)
    in
    units, diagnostics, expanded_trees

let expand_only
    : iri -> transition
  = fun iri forest ->
    Eio.traceln "Expanding %a" pp_iri iri;
    let units, new_diagnostics, trees =
      expand_only_aux
        ~quit_on_error: false
        ~addr: iri
        forest
    in
    trees
    |> Forest.iter
      (
        fun iri tree ->
          Forest.replace forest.expanded iri tree;
          match Iri_resolver.resolve (Iri iri) To_uri forest with
          | None -> ()
          | Some _uri -> ()
        (* Eio.traceln "clearing diagnostics for %s" (Lsp.Uri.to_string uri); *)
        (* Diagnostics.remove forest.diagnostics uri *)
      );
    new_diagnostics
    |> Diagnostic_store.iter
      (
        fun uri diagnostics ->
          Eio.traceln "got some expansion diagnostics.";
          match diagnostics with
          | [] -> ()
          | diagnostics ->
            Diagnostic_store.append forest.diagnostics uri diagnostics
      );
    { forest with units }

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
    Eio.Path.mkdirs ~exists_ok: true ~perm: 0o755 dir;
    Eio.Path.save
      ~create: (`Or_truncate 0o644)
      Eio.Path.(dir / filename)
      blob

let eval
    : dev: bool -> transition
  = fun ~dev forest ->
    let host = forest.config.host in
    let expanded = forest.expanded in
    let env = forest.env in
    expanded
    |> Forest.iter
      (
        fun iri syn ->
          let source_path = if dev then Iri_resolver.(resolve (Iri iri) To_path forest) else None in
          let uri = Iri_resolver.(resolve (Iri iri) To_uri forest) in
          let diagnostics, Eval.{ articles; jobs } =
            Eval.eval_tree
              ~quit_on_failure: false
              ~host: forest.config.host
              ~source_path
              ~iri
              syn
          in
          begin
            match uri with
            | None -> ()
            | Some uri ->
              Diagnostic_store.append forest.diagnostics uri diagnostics
          end;
          articles
          |> List.iter
            (
              fun article ->
                let resource = (T.Article article) in
                match Forest.iri_for_resource resource with
                | Some iri ->
                  Forest.add forest.resources iri resource
                | None ->
                  Logs.debug (fun m -> m "failed to plant resource because an iri could not be guessed")
            );
          jobs
          |> List.iter
            (
              fun Range.{ value; loc } ->
                match value with
                | Job.LaTeX_to_svg { hash; source; content } ->
                  let svg = Build_latex.latex_to_svg ~env ?loc source in
                  let iri = Iri_scheme.hash_iri ~host hash in
                  let frontmatter = T.default_frontmatter ~iri () in
                  let mainmatter = content ~svg in
                  let backmatter = T.Content [] in
                  let article = T.{ frontmatter; mainmatter; backmatter } in
                  Forest.add forest.resources iri (T.Article article)
                | Job.Publish publication ->
                  export_publication ~env ~forest publication
            )
      );
    forest

let eval_only : iri -> transition = fun iri forest ->
    match Forest.find_opt forest.resources iri with
    | None -> assert false
    | Some _ -> forest
