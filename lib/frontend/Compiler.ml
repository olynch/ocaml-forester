(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Forester_forest

module T = Types
module EP = Eio.Path

type resource = T.content T.resource

type state = State.t

let documents : state -> _ Hashtbl.t = fun state -> state.documents
let parsed : state -> _ Forest.t = fun state -> state.parsed
let resources : state -> _ Forest.t = fun state -> state.resources
let expanded : state -> _ Forest.t = fun state -> state.expanded
let graphs : state -> (module Forest_graphs.S) = fun state -> state.graphs
let get_config : state -> Config.Forest_config.t = fun state -> state.config
let get_env : state -> Eio_unix.Stdenv.base = fun state -> state.env
let get_diagnostics : state -> Diagnostics.table = fun state -> state.diagnostics
let units : state -> Expand.Env.t = fun state -> state.units

let with_config : Config.Forest_config.t -> state -> state = fun config forest -> { forest with config }
let with_expanded : Syn.t Forest.t -> state -> state = fun expanded forest -> { forest with expanded }

let init
    : env: Eio_unix.Stdenv.base -> config: Config.Forest_config.t -> state
  = fun ~env ~config ->
    let graphs = (module Forest_graphs.Make (): Forest_graphs.S) in
    let parsed = Forest.create 1000 in
    let documents = Hashtbl.create 1000 in
    let expanded = Forest.create 1000 in
    let resources = Forest.create 1000 in
    let diagnostics = Diagnostics.create 100 in
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

let get_all_articles (forest : state) =
  forest.resources
  |> Forest.to_seq_values
  |> Seq.filter_map
    (
      fun r ->
        match r with
        | T.Article a -> Some a
        | T.Asset _ -> None
    )
  |> List.of_seq

let get_all_resources (forest : state) =
  forest.resources
  |> Forest.to_seq_values
  |> List.of_seq

let get_article
    : iri -> state -> T.content T.article option
  = fun iri forest ->
    let tbl = forest.resources in
    match Forest.find_opt tbl iri with
    | None -> None
    | Some (T.Asset _) ->
      Logs.debug (fun m -> m "%a is an asset, not an article" pp_iri iri);
      None
    | Some (T.Article article) -> Some article

(*  TODO: after my refactors, it is no longer clear when and how this should be
    used. I think we need to find a definitive design for when to call
    register_iri, and no longer use a name such as plant_resource. It seems to
    me that this function conflates adding stuff to the hash table and dealing
    with the graphs.*)

let plant_resource
    : resource -> state -> unit
  = fun resource forest ->
    let module Graphs = (val forest.graphs) in
    Forest.analyse_resource forest.graphs resource;
    let resources = forest.resources in
    let@ iri = Option.iter @~ Forest.iri_for_resource resource in
    let iri = Iri.normalize iri in
    Graphs.register_iri iri;
    match Forest.mem resources iri with
    | false ->
      (* Graphs.register_iri iri; *)
      Forest.add resources iri resource
    | true ->
      ()

let load
    : Eio.Fs.dir_ty EP.t list ->
    state ->
    state
  = fun tree_dirs forest ->
    Logs.debug (fun m -> m "loading trees from file system");
    (* let host = forest.config.host in *)
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

let parse
    : quit_on_error: bool -> state -> state
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
                Diagnostics.replace forest.diagnostics uri [diagnostic];
            | Ok tree -> Forest.add forest.parsed iri tree
          end;
      );
    let parsed = forest.parsed |> Forest.length in
    Logs.debug (fun m -> m "parsed %i trees" parsed);
    forest

(* TODO: Amend import graph *)
let reparse
    : Lsp.Text_document.t -> state -> state
  = fun doc forest ->
    Eio.traceln "reparsing";
    (* let text = Lsp.Text_document.text doc in *)
    let host = forest.config.host in
    let uri = Lsp.Text_document.documentUri doc in
    let path = Lsp.Uri.to_path uri in
    (* Hashtbl.replace forest.documents uri doc; *)
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
      Diagnostics.remove forest.diagnostics uri;
      forest
    | Error d ->
      (* When we get a parsing diagnostic, we don't need to merge the value with the diagnostics from previous passes*)
      Diagnostics.replace forest.diagnostics uri [d];
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
      match Iri.host addr with
      | None -> forest
      | Some host ->
        let module Graphs = (val forest.graphs) in
        let import_graph = Imports.dependencies ~host tree forest in
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
              Diagnostics.append forest.diagnostics uri diagnostics
      );
    let expanded = forest.expanded |> Forest.length in
    let diagnostics = forest.diagnostics |> Diagnostics.length in
    Logs.debug (fun m -> m "expanded %i trees" expanded);
    Logs.debug (fun m -> m "%i trees emitted diagnostics." diagnostics);
    forest

let expand_only_aux
    : quit_on_error: bool ->
    addr: iri ->
    State.t ->
    Expand.Env.t * Diagnostics.table * Syn.tree Forest.t
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
                Diagnostics.add diagnostics uri ds
          end;
          units, diagnostics, trees
    in
    let units, diagnostics, expanded_trees =
      Eio.traceln "Folding the import graph with %i vertices" (Forest_graph.nb_vertex import_graph);
      Forest_graph.topo_fold
        task
        import_graph
        (Expand.Env.empty, Diagnostics.create 100, Forest.create 1000)
    in
    units, diagnostics, expanded_trees

let expand_only
    : iri -> state -> state
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
    |> Diagnostics.iter
      (
        fun uri diagnostics ->
          Eio.traceln "got some expansion diagnostics.";
          match diagnostics with
          | [] -> ()
          | diagnostics ->
            Diagnostics.append forest.diagnostics uri diagnostics
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
    : dev: bool -> state -> state
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
              Diagnostics.append forest.diagnostics uri diagnostics
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
    let resources = forest.resources |> Forest.length in
    Logs.debug (fun m -> m "evaluated %i trees" resources);
    forest

let plant
    : State.t -> State.t
  = fun forest ->
    let resources = get_all_resources forest in
    List.iter (fun t -> plant_resource t forest) resources;
    forest

let section_symbol = "§"

let rec get_expanded_title ?scope ?(flags = T.{ empty_when_untitled = false }) (frontmatter : _ T.frontmatter) forest =
  let short_title =
    match frontmatter.title with
    | Some content -> content
    | None when not flags.empty_when_untitled ->
      begin
        match frontmatter.iri with
        | Some iri -> T.Content [T.Iri iri]
        | _ -> T.Content [T.Text "Untitled"]
      end
    | _ -> T.Content []
  in
  Option.value ~default: short_title @@
    match frontmatter.designated_parent with
    | Some parent_iri when not (scope = frontmatter.designated_parent) ->
      let@ parent = Option.map @~ get_article parent_iri forest in
      let parent_title = get_expanded_title parent.frontmatter forest in
      let parent_link = T.Link { href = parent_iri; content = parent_title } in
      let chevron = T.Text " › " in
      T.map_content (fun xs -> parent_link :: chevron :: xs) short_title
    | _ -> None

let get_title_or_content_of_vertex ?(not_found = fun _ -> None) ~modifier vertex forest =
  let@ content =
    Option.map @~
      match vertex with
      | T.Content_vertex content -> Some content
      | T.Iri_vertex iri ->
        begin
          match get_article iri forest with
          | Some article -> article.frontmatter.title
          | None -> not_found iri
        end
  in
  T.apply_modifier_to_content modifier content

let get_content_of_transclusion (transclusion : T.content T.transclusion) forest =
  let@ content =
    Option.map @~
      match transclusion.target with
      | Full flags ->
        let@ article = Option.map @~ get_article transclusion.href forest in
        T.Content [T.Section (T.article_to_section article ~flags)]
      | Mainmatter ->
        let@ article = Option.map @~ get_article transclusion.href forest in
        article.mainmatter
      | Title flags ->
        Option.some @@
          begin
            match get_article transclusion.href forest with
            | None -> T.Content [T.Iri transclusion.href]
            | Some article -> get_expanded_title ~flags article.frontmatter forest
          end
      | Taxon ->
        let@ article = Option.map @~ get_article transclusion.href forest in
        let default = T.Content [T.Text section_symbol] in
        Option.value ~default article.frontmatter.taxon
  in
  T.apply_modifier_to_content transclusion.modifier content
