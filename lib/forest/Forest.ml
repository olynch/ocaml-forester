(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

module T = Types
module Q = Query

type resource = T.content T.resource

module type S = sig
  val plant_resource : resource -> unit

  val get_resource : Iri.t -> resource option
  val get_article : Iri.t -> T.content T.article option
  val get_all_resources : unit -> resource Seq.t

  val get_expanded_title : ?scope: iri -> ?flags: T.title_flags -> T.content T.frontmatter -> T.content
  val get_content_of_transclusion : T.content T.transclusion -> T.content option
  val get_title_or_content_of_vertex : ?not_found: (iri -> T.content option) -> modifier: T.modifier -> T.content T.vertex -> T.content option

  val run_datalog_query : (string, Vertex.t) Datalog_expr.query -> Vertex_set.t
  val run_query : T.query -> Vertex_set.t
end

module Make (Graphs: Forest_graphs.S) : S = struct
  type article = T.content T.article

  module Dci = Datalog_engine
  module Dx = Datalog_expr

  let execute_datalog_script script =
    let@ sequent = List.iter @~ script in
    Dci.db_add Graphs.dl_db (Datalog_eval.eval_sequent sequent)

  let () = execute_datalog_script Builtin_relation.axioms

  let run_datalog_query =
    Datalog_eval.run_query Graphs.dl_db

  let resources : (Iri.t, resource) Hashtbl.t =
    Hashtbl.create 1000

  let add_edge rel ~source ~target =
    let premises = [] in
    let conclusion =
      let args = [Dx.Const source; Dx.Const target] in
      Dx.{ rel; args }
    in
    execute_datalog_script [{ conclusion; premises }];
    Graphs.add_edge rel ~source ~target

  let rec analyse_content_node (scope : Iri.t) (node : 'a T.content_node) : unit =
    match node with
    | Text _ | CDATA _ | Route_of_iri _ | Iri _ | Results_of_query _ | Results_of_datalog_query _ | TeX_cs _ | Img _ | Contextual_number _ -> ()
    | Transclude transclusion ->
      analyse_transclusion scope transclusion
    | Xml_elt elt ->
      begin
        let@ attr = List.iter @~ elt.attrs in
        analyse_content scope attr.value
      end;
      analyse_content scope elt.content
    | Section section ->
      analyse_section scope section
    | Link link ->
      add_edge Builtin_relation.links_to ~source: (Iri_vertex scope) ~target: (Iri_vertex link.href);
      analyse_content scope link.content
    | Prim (_, content) ->
      analyse_content scope content
    | KaTeX (_, content) ->
      analyse_content scope content
    | Artefact artefact ->
      analyse_artefact scope artefact
    | Datalog_script script ->
      execute_datalog_script script

  and analyse_artefact scope artefact =
    analyse_content scope artefact.content

  and analyse_transclusion (scope : Iri.t) (transclusion : T.content T.transclusion) : unit =
    match transclusion.target with
    | Full _ | Mainmatter ->
      add_edge Builtin_relation.transcludes ~source: (Iri_vertex scope) ~target: (Iri_vertex transclusion.href)
    | Title _ | Taxon -> ()

  and analyse_content (scope : Iri.t) (content : T.content) : unit =
    T.extract_content content |> List.iter @@ analyse_content_node scope

  and analyse_attribution (scope : Iri.t) (attr : _ T.attribution) =
    let rel =
      match attr.role with
      | Author -> Builtin_relation.has_author
      | Contributor -> Builtin_relation.has_direct_contributor
    in
    add_edge rel ~source: (Iri_vertex scope) ~target: attr.vertex;
    analyse_vertex scope attr.vertex

  and analyse_vertex scope = function
    | Iri_vertex _ -> ()
    | Content_vertex content -> analyse_content scope content

  and analyse_tag (scope : Iri.t) (tag : _ T.vertex) =
    analyse_vertex scope tag;
    add_edge Builtin_relation.has_tag ~source: (Iri_vertex scope) ~target: tag

  and analyse_taxon (scope : Iri.t) (taxon_opt : T.content option) =
    let@ taxon = Option.iter @~ taxon_opt in
    analyse_content scope taxon;
    add_edge Builtin_relation.has_taxon ~source: (Iri_vertex scope) ~target: (Content_vertex taxon)

  and analyse_attributions (scope : Iri.t) (attrs : _ T.attribution list) =
    attrs |> List.iter @@ analyse_attribution scope

  and analyse_tags (scope : Iri.t) (tags : _ T.vertex list) =
    tags |> List.iter @@ analyse_tag scope

  and analyse_frontmatter (fm : T.content T.frontmatter) : unit =
    let@ scope = Option.iter @~ fm.iri in
    Option.iter (analyse_content scope) fm.title;
    analyse_taxon scope fm.taxon;
    analyse_attributions scope fm.attributions;
    analyse_tags scope fm.tags;
    analyse_metas scope fm.metas

  and analyse_metas (scope : Iri.t) (metas : (string * T.content) list) : unit =
    metas |> List.iter @@ analyse_meta scope

  and analyse_meta (scope : Iri.t) (_, content) : unit =
    analyse_content scope content

  and analyse_section (scope : Iri.t) (section : T.content T.section) : unit =
    begin
      let@ target = Option.iter @~ section.frontmatter.iri in
      add_edge Builtin_relation.transcludes ~source: (Iri_vertex scope) ~target: (Iri_vertex target)
    end;
    analyse_frontmatter section.frontmatter;
    analyse_content (Option.value ~default: scope section.frontmatter.iri) section.mainmatter

  let analyse_article (article : article) : unit =
    analyse_frontmatter article.frontmatter;
    let@ scope = Option.iter @~ article.frontmatter.iri in
    analyse_content scope article.mainmatter;
    analyse_content scope article.backmatter

  let analyse_resource = function
    | T.Article article -> analyse_article article
    | T.Asset _ -> ()

  let iri_for_resource = function
    | T.Article article -> article.frontmatter.iri
    | T.Asset asset -> Some asset.iri

  let plant_resource resource =
    analyse_resource resource;
    let@ iri = Option.iter @~ iri_for_resource resource in
    let iri = Iri.normalize iri in
    match Hashtbl.mem resources iri with
    | false ->
      Graphs.register_iri iri;
      Hashtbl.add resources iri resource
    | true ->
      ()

  let get_resource iri =
    let iri = Iri.normalize iri in
    Hashtbl.find_opt resources iri

  let get_all_resources () =
    Hashtbl.to_seq_values resources

  let get_article iri =
    let@ resource = Option.bind @@ get_resource iri in
    match resource with
    | Article article -> Some article
    | _ -> None

  let get_article_exn addr =
    match get_article addr with
    | Some article -> article
    | None ->
      Reporter.fatalf Resource_not_found "Could not find tree %a" pp_iri addr

  module Legacy_query_engine = Legacy_query_engine.Make(Graphs)
  include Legacy_query_engine

  let section_symbol = "§"

  let rec get_expanded_title ?scope ?(flags = T.{ empty_when_untitled = false }) (frontmatter : _ T.frontmatter) =
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
        let@ parent = Option.map @~ get_article parent_iri in
        let parent_title = get_expanded_title parent.frontmatter in
        let parent_link = T.Link { href = parent_iri; content = parent_title } in
        let chevron = T.Text " › " in
        T.map_content (fun xs -> parent_link :: chevron :: xs) short_title
      | _ -> None

  let get_title_or_content_of_vertex ?(not_found = fun _ -> None) ~modifier vertex =
    let@ content =
      Option.map @~
        match vertex with
        | T.Content_vertex content -> Some content
        | T.Iri_vertex iri ->
          begin
            match get_article iri with
            | Some article -> article.frontmatter.title
            | None -> not_found iri
          end
    in
    T.apply_modifier_to_content modifier content

  let get_content_of_transclusion (transclusion : T.content T.transclusion) =
    let@ content =
      Option.map @~
        match transclusion.target with
        | Full flags ->
          let@ article = Option.map @~ get_article transclusion.href in
          T.Content [T.Section (T.article_to_section article ~flags)]
        | Mainmatter ->
          let@ article = Option.map @~ get_article transclusion.href in
          article.mainmatter
        | Title flags ->
          Option.some @@
            begin
              match get_article transclusion.href with
              | None -> T.Content [T.Iri transclusion.href]
              | Some article -> get_expanded_title ~flags article.frontmatter
            end
        | Taxon ->
          let@ article = Option.map @~ get_article transclusion.href in
          let default = T.Content [T.Text section_symbol] in
          Option.value ~default article.frontmatter.taxon
    in
    T.apply_modifier_to_content transclusion.modifier content
end
