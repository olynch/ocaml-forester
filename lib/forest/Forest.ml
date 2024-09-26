open Forester_prelude
open Forester_core

module T = Xml_tree
module Q = Query

module type S = sig
  val plant_article : T.content T.article -> unit
  val get_article : Iri.t -> T.content T.article option

  val get_expanded_title : ?scope: iri -> T.content T.frontmatter -> T.content
  val get_content_of_transclusion : T.content T.transclusion -> T.content
  val get_title_or_content_of_vertex : ?not_found: (iri -> T.content option) -> modifier: T.modifier -> T.content T.vertex -> T.content option
  val run_query : T.query -> Vertex_set.t
end

module Make (Graphs: Forest_graphs.S) : S = struct
  type article = T.content T.article
  let articles : (Iri.t, article) Hashtbl.t =
    Hashtbl.create 1000

  let rec analyse_content_node (scope : Iri.t) (node : 'a T.content_node) : unit =
    match node with
    | Text _ | CDATA _ | Results_of_query _ | TeX_cs _ | Img _ | Contextual_number _ -> ()
    | Transclude transclusion ->
      analyse_transclusion scope transclusion
    | Xml_elt elt ->
      analyse_content scope elt.content
    | Section section ->
      analyse_section scope section
    | Link link ->
      Graphs.add_edge Q.Rel.links ~source: (Iri_vertex scope) ~target: (Iri_vertex link.href);
      analyse_content scope link.content
    | Prim (_, content) ->
      analyse_content scope content
    | KaTeX (_, content) ->
      analyse_content scope content
    | Resource resource ->
      analyse_resource scope resource

  and analyse_resource scope resource =
    analyse_content scope resource.content

  and analyse_transclusion (scope : Iri.t) (transclusion : T.content T.transclusion) : unit =
    match transclusion.target with
    | Full _ | Mainmatter ->
      Graphs.add_edge Q.Rel.transclusion ~source: (Iri_vertex scope) ~target: (Iri_vertex transclusion.href)
    | Title | Taxon -> ()

  and analyse_content (scope : Iri.t) (content : T.content) : unit =
    T.extract_content content |> List.iter @@ analyse_content_node scope

  and analyse_attribution (scope : Iri.t) (attr : _ T.attribution) =
    let rel =
      match attr.role with
      | Author -> Q.Rel.authors
      | Contributor -> Q.Rel.contributors
    in
    Graphs.add_edge rel ~source: (Iri_vertex scope) ~target: attr.vertex;
    analyse_vertex scope attr.vertex

  and analyse_vertex scope = function
    | Iri_vertex _ -> ()
    | Content_vertex content -> analyse_content scope content

  and analyse_tag (scope : Iri.t) (tag : _ T.vertex) =
    analyse_vertex scope tag;
    Graphs.add_edge Q.Rel.tags ~source: (Iri_vertex scope) ~target: tag

  and analyse_taxon (scope : Iri.t) (taxon_opt : T.content option) =
    let@ taxon = Option.iter @~ taxon_opt in
    analyse_content scope taxon;
    Graphs.add_edge Q.Rel.taxa ~source: (Iri_vertex scope) ~target: (Content_vertex taxon)

  and analyse_attributions (scope : Iri.t) (attrs : _ T.attribution list) =
    attrs |> List.iter @@ analyse_attribution scope

  and analyse_tags (scope : Iri.t) (tags : _ T.vertex list) =
    tags |> List.iter @@ analyse_tag scope

  and analyse_frontmatter (fm : T.content T.frontmatter) : unit =
    let@ scope = Option.iter @~ fm.iri in
    Graphs.register_iri scope;
    analyse_content scope fm.title;
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
      Graphs.add_edge Q.Rel.transclusion ~source: (Iri_vertex scope) ~target: (Iri_vertex target)
    end;
    analyse_frontmatter section.frontmatter;
    analyse_content (Option.value ~default: scope section.frontmatter.iri) section.mainmatter

  let analyse_article (article : article) : unit =
    analyse_frontmatter article.frontmatter;
    let@ scope = Option.iter @~ article.frontmatter.iri in
    analyse_content scope article.mainmatter;
    analyse_content scope article.backmatter

  let plant_article (article : article) : unit =
    analyse_article article;
    let@ iri = Option.iter @~ article.frontmatter.iri in
    match Hashtbl.mem articles iri with
    | false ->
      Hashtbl.add articles iri article
    | true ->
      Reporter.emitf Duplicate_tree "Already planted tree at address %a" pp_iri iri

  let get_article addr =
    Hashtbl.find_opt articles addr

  let get_article_exn addr =
    match get_article addr with
    | Some article -> article
    | None ->
      Reporter.fatalf Tree_not_found "Could not find tree %a" pp_iri addr

  module Query_engine = Query_engine.Make(Graphs)
  include Query_engine

  let section_symbol = "§"

  let rec get_expanded_title ?scope (frontmatter : _ T.frontmatter) =
    let short_title = frontmatter.title in
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
            | Some article -> Some article.frontmatter.title
            | None -> not_found iri
          end
    in
    T.apply_modifier_to_content modifier content

  let get_content_of_transclusion (transclusion : T.content T.transclusion) =
    let content =
      match transclusion.target with
      | T.Full flags ->
        let article = get_article_exn transclusion.href in
        T.Content [T.Section (T.article_to_section article ~flags)]
      | Mainmatter ->
        let article = get_article_exn transclusion.href in
        article.mainmatter
      | Title ->
        begin
          match get_article transclusion.href with
          | None -> T.Content [T.Text (Format.asprintf "%a" pp_iri transclusion.href)]
          | Some article -> get_expanded_title article.frontmatter
        end
      | Taxon ->
        let article = get_article_exn transclusion.href in
        let default = T.Content [T.Text section_symbol] in
        Option.value ~default article.frontmatter.taxon
    in
    T.apply_modifier_to_content transclusion.modifier content
end
