(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

module T = Types
module Q = Query

module Tbl = Hashtbl.Make(struct
  type t = iri
  let equal = Iri.equal ~normalize: true
  let hash iri = Iri.normalize iri |> Hashtbl.hash
end)

include Tbl

let iri_for_resource = function
  | T.Article article -> article.frontmatter.iri
  | T.Asset asset -> Some asset.iri

type article = T.content T.article

module Dx = Datalog_expr

type env = (module Forest_graphs.S)

let legacy_query_engine
    : env -> (module Legacy_query_engine.S)
  = fun env ->
    let module Graphs = (val env) in
    (module Legacy_query_engine.Make(Graphs))

let execute_datalog_script graphs script =
  let module Graphs = (val graphs : Forest_graphs.S) in
  let@ sequent = List.iter @~ script in
  Datalog_engine.db_add Graphs.dl_db (Datalog_eval.eval_sequent sequent)

(* let () = execute_datalog_script Builtin_relation.axioms *)

let run_datalog_query
    : env -> (string, Vertex.t) Dx.query -> Vertex_set.t
  = fun graphs q ->
    let () = execute_datalog_script graphs Builtin_relation.axioms in
    let module Graphs = (val graphs) in
    Datalog_eval.run_query Graphs.dl_db q

let add_edge graphs rel ~source ~target =
  let module Graphs = (val graphs : Forest_graphs.S) in
  let premises = [] in
  let conclusion =
    let args = [Dx.Const source; Dx.Const target] in
    Dx.{ rel; args }
  in
  execute_datalog_script graphs [{ conclusion; premises }];
  Graphs.add_edge rel ~source ~target

let rec analyse_content_node graphs (scope : Iri.t) (node : 'a T.content_node) : unit =
  match node with
  | Text _ | CDATA _ | Route_of_iri _ | Iri _ | Results_of_query _ | Results_of_datalog_query _ | TeX_cs _ | Img _ | Contextual_number _ -> ()
  | Transclude transclusion ->
    analyse_transclusion graphs scope transclusion
  | Xml_elt elt ->
    begin
      let@ attr = List.iter @~ elt.attrs in
      analyse_content graphs scope attr.value
    end;
    analyse_content graphs scope elt.content
  | Section section ->
    analyse_section graphs scope section
  | Link link ->
    add_edge graphs Builtin_relation.links_to ~source: (Iri_vertex scope) ~target: (Iri_vertex link.href);
    analyse_content graphs scope link.content
  | Prim (_, content) ->
    analyse_content graphs scope content
  | KaTeX (_, content) ->
    analyse_content graphs scope content
  | Artefact artefact ->
    analyse_artefact graphs scope artefact
  | Datalog_script script ->
    execute_datalog_script graphs script

and analyse_artefact graphs scope artefact =
  analyse_content graphs scope artefact.content

and analyse_transclusion graphs (scope : Iri.t) (transclusion : T.content T.transclusion) : unit =
  match transclusion.target with
  | Full _ | Mainmatter ->
    add_edge graphs Builtin_relation.transcludes ~source: (Iri_vertex scope) ~target: (Iri_vertex transclusion.href)
  | Title _ | Taxon -> ()

and analyse_content (graphs : env) (scope : Iri.t) (content : T.content) : unit =
  T.extract_content content |> List.iter @@ (analyse_content_node graphs scope)

and analyse_attribution graphs (scope : Iri.t) (attr : _ T.attribution) =
  let rel =
    match attr.role with
    | Author -> Builtin_relation.has_author
    | Contributor -> Builtin_relation.has_direct_contributor
  in
  add_edge graphs rel ~source: (Iri_vertex scope) ~target: attr.vertex;
  analyse_vertex graphs scope attr.vertex

and analyse_vertex graphs scope vtx =
  match vtx with
  | Iri_vertex _ -> ()
  | Content_vertex content -> analyse_content graphs scope content

and analyse_tag graphs (scope : Iri.t) (tag : _ T.vertex) =
  analyse_vertex graphs scope tag;
  add_edge graphs Builtin_relation.has_tag ~source: (Iri_vertex scope) ~target: tag

and analyse_taxon graphs (scope : Iri.t) (taxon_opt : T.content option) =
  let@ taxon = Option.iter @~ taxon_opt in
  analyse_content graphs scope taxon;
  add_edge graphs Builtin_relation.has_taxon ~source: (Iri_vertex scope) ~target: (Content_vertex taxon)

and analyse_attributions graphs (scope : Iri.t) (attrs : _ T.attribution list) =
  attrs |> List.iter @@ analyse_attribution graphs scope

and analyse_tags graphs (scope : Iri.t) (tags : _ T.vertex list) =
  tags |> List.iter @@ analyse_tag graphs scope

and analyse_frontmatter graphs (fm : T.content T.frontmatter) : unit =
  let@ scope = Option.iter @~ fm.iri in
  Option.iter (analyse_content graphs scope) fm.title;
  analyse_taxon graphs scope fm.taxon;
  analyse_attributions graphs scope fm.attributions;
  analyse_tags graphs scope fm.tags;
  analyse_metas graphs scope fm.metas

and analyse_metas graphs (scope : Iri.t) (metas : (string * T.content) list) : unit =
  metas |> List.iter @@ analyse_meta graphs scope

and analyse_meta graphs (scope : Iri.t) (_, content) : unit =
  analyse_content graphs scope content

and analyse_section graphs (scope : Iri.t) (section : T.content T.section) : unit =
  begin
    let@ target = Option.iter @~ section.frontmatter.iri in
    add_edge graphs Builtin_relation.transcludes ~source: (Iri_vertex scope) ~target: (Iri_vertex target)
  end;
  analyse_frontmatter graphs section.frontmatter;
  analyse_content graphs (Option.value ~default: scope section.frontmatter.iri) section.mainmatter

let analyse_article graphs (article : article) : unit =
  analyse_frontmatter graphs article.frontmatter;
  let@ scope = Option.iter @~ article.frontmatter.iri in
  analyse_content graphs scope article.mainmatter;
  analyse_content graphs scope article.backmatter

let analyse_resource graphs = function
  | T.Article article -> analyse_article graphs article
  | T.Asset _ -> ()
