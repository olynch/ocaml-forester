(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

module T = Types
module Q = Query

(* Ought to use Iri_hash, but I am optimising by avoiding noramlizing on equality comparison. *)
module Tbl = Hashtbl.Make(struct
  type t = iri
  let equal = Iri.equal ~normalize: false

  (* IRI has mutable state that seems to be interfering with Hashtbl.hash *)
  let clean iri = Iri.with_query iri (Iri.query iri)

  (* If I remove the normalization here, everything fails. But I can't figure out why! *)
  let hash iri = clean iri |> Hashtbl.hash
end)

type 'a t = 'a Tbl.t
type key = iri

let find_opt tbl iri =
  Tbl.find_opt tbl (Iri.normalize iri)

let mem tbl iri =
  Tbl.mem tbl (Iri.normalize iri)

let add tbl iri x =
  Tbl.add tbl (Iri.normalize iri) x

let replace tbl iri x =
  Tbl.replace tbl (Iri.normalize iri) x

let length = Tbl.length
let create = Tbl.create
let iter = Tbl.iter
let to_seq_values = Tbl.to_seq_values
let to_seq_keys = Tbl.to_seq_keys

type resource = T.content T.resource

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

let get_article
    : iri -> _ t -> T.content T.article option
  = fun iri forest ->
    match find_opt forest iri with
    | None -> None
    | Some (T.Asset _) ->
      Logs.debug (fun m -> m "%a is an asset, not an article" pp_iri iri);
      None
    | Some (T.Article article) -> Some article

let plant_resource
    : resource -> (module Forest_graphs.S) -> resource t -> unit
  = fun resource graphs resources ->
    let module Graphs = (val graphs) in
    analyse_resource graphs resource;
    let@ iri = Option.iter @~ iri_for_resource resource in
    let iri = Iri.normalize iri in
    Graphs.register_iri iri;
    match mem resources iri with
    | false ->
      (* Graphs.register_iri iri; *)
      add resources iri resource
    | true ->
      ()

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

let section_symbol = "§"

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

let get_all_articles resources =
  let extract_article = function
    | T.Article a -> Some a
    | T.Asset _ -> None
  in
  resources
  |> to_seq_values
  |> Seq.filter_map extract_article
  |> List.of_seq

let get_all_assets resources =
  let extract_asset = function
    | T.Asset a -> Some a
    | T.Article _ -> None
  in
  resources
  |> to_seq_values
  |> Seq.filter_map extract_asset
  |> List.of_seq

let get_all_resources resources =
  resources
  |> to_seq_values
  |> List.of_seq
