open Forester_prelude
open Forester_core

module T = Xml_tree
module Q = Query


module type S = sig
  val plant_article : T.content T.article -> unit
  val get_article : addr -> T.content T.article option

  val get_expanded_title : ?scope:addr option -> T.content T.frontmatter -> T.content
  val get_content_of_transclusion : T.content T.transclusion -> T.content
  val run_query : Query.dbix Query.expr -> Addr_set.t
end

module Make (Graphs : Forester_graphs.S) : S =
struct
  type article = T.content T.article
  let articles : (addr, article) Hashtbl.t =
    Hashtbl.create 1000

  let rec analyse_content_node (scope : addr) (node : T.content_node) : unit =
    match node with
    | Text _ | CDATA _ | Results_of_query _ | TeX_cs _ | Img _ -> ()
    | Transclude transclusion ->
      analyse_transclusion scope transclusion
    | Xml_elt elt ->
      analyse_content scope elt.content
    | Section section ->
      analyse_section scope section
    | Link link ->
      Graphs.add_edge Q.Rel.links ~source:scope ~target:(User_addr link.href);
      analyse_content scope link.content
    | Prim (p, content) ->
      analyse_content scope content
    | KaTeX (_, content) ->
      analyse_content scope content
    | Resource resource ->
      analyse_resource scope resource

  and analyse_resource scope resource =
    analyse_content scope resource.content

  and analyse_transclusion (scope : addr) (transclusion : T.content T.transclusion) : unit =
    match transclusion.target with
    | Full _ | Mainmatter ->
      Graphs.add_edge Q.Rel.transclusion ~source:scope ~target:transclusion.addr
    | Title | Taxon | Number -> ()

  and analyse_content (scope : addr) (content : T.content) : unit =
    content |> List.iter @@ analyse_content_node scope

  and analyse_attribution (scope : addr) (attr : T.attribution) =
    match attr with
    | Author author ->
      Graphs.add_edge Q.Rel.authors ~source:scope ~target:(User_addr author);
    | Contributor contributor ->
      Graphs.add_edge Q.Rel.contributors ~source:scope ~target:(User_addr contributor)

  and analyse_tag (scope : addr) (tag : string) =
    Graphs.add_edge Q.Rel.tags ~source:scope ~target:(User_addr tag)

  and analyse_taxon (scope : addr) (taxon_opt : string option) =
    taxon_opt |> Option.iter @@ fun taxon ->
    Graphs.add_edge Q.Rel.taxa ~source:scope ~target:(User_addr taxon)

  and analyse_attributions (scope : addr) (attrs : T.attribution list) =
    attrs |> List.iter @@ analyse_attribution scope

  and analyse_tags (scope : addr) (tags : string list) =
    tags |> List.iter @@ analyse_tag scope

  and analyse_frontmatter (fm : T.content T.frontmatter) : unit =
    Graphs.register_addr fm.addr;
    analyse_content fm.addr fm.title;
    analyse_taxon fm.addr fm.taxon;
    analyse_attributions fm.addr fm.attributions;
    analyse_tags fm.addr fm.tags

  and analyse_section (scope : addr) (section : T.content T.section) : unit =
    Graphs.add_edge Q.Rel.transclusion ~source:scope ~target:section.frontmatter.addr;
    analyse_frontmatter section.frontmatter;
    analyse_content section.frontmatter.addr section.mainmatter

  let analyse_article (article : article) : unit =
    analyse_frontmatter article.frontmatter;
    analyse_content article.frontmatter.addr article.mainmatter;
    analyse_content article.frontmatter.addr article.backmatter

  let plant_article (article : article) : unit =
    analyse_article article;
    Hashtbl.add articles article.frontmatter.addr article

  let get_article addr =
    Hashtbl.find_opt articles addr

  let get_article_exn addr =
    match get_article addr with
    | Some article -> article
    | None ->
      Reporter.fatalf Tree_not_found "Could not find tree %a" pp_addr addr


  module Query_engine = Query_engine.Make (Graphs)
  include Query_engine


  let section_symbol = "§"

  let rec get_expanded_title ?(scope = None) (frontmatter : _ T.frontmatter) =
    let short_title = frontmatter.title in
    match frontmatter.designated_parent with
    | Some (User_addr parent_addr) when not (scope = frontmatter.designated_parent) ->
      begin
        match get_article @@ User_addr parent_addr with
        | None -> short_title
        | Some parent ->
          let parent_title = get_expanded_title parent.frontmatter in
          let parent_link = T.Link {href = parent_addr; content = parent_title} in
          let chevron = T.Text " › " in
          parent_link :: chevron :: short_title
      end

    | _ -> short_title


  let get_content_of_transclusion (transclusion : T.content T.transclusion) =
    let content =
      match transclusion.target with
      | Full (flags, overrides) ->
        let article = get_article_exn transclusion.addr in
        [T.Section (T.article_to_section article ~flags ~overrides)]
      | Mainmatter ->
        let article = get_article_exn transclusion.addr in
        article.mainmatter
      | Title ->
        begin
          match get_article transclusion.addr with
          | None -> [T.Text (Format.asprintf "%a" pp_addr transclusion.addr)]
          | Some article -> get_expanded_title ~scope:None article.frontmatter
        end
      | Taxon ->
        let article = get_article_exn transclusion.addr in
        let taxon = Option.value ~default:section_symbol article.frontmatter.taxon in
        [T.Text taxon]
      | Number ->
        let article = get_article_exn transclusion.addr in
        let number =
          match article.frontmatter.number with
          | Some number -> number
          | None -> Format.asprintf "[%a]" pp_addr article.frontmatter.addr
        in
        [T.Text number]
    in
    T.apply_modifier_to_content transclusion.modifier content
end

