open Forester_prelude
open Forester_xml_names
open Forester_core

module T = Types
module P = Pure_html
module X = Xml_forester

module type S = sig
  val route : Iri.t -> string
  val render_article : T.content T.article -> P.node

  val pp_xml : ?stylesheet: string -> Format.formatter -> T.content T.article -> unit
end

module type Params = sig
  val host : string
  val home : string option
end

module Make (Params: Params) (F: Forest.S) () : S = struct

  module Util = Forest_util.Make(F)

  module Xmlns = struct
    include Xmlns_effect.Make ()

    let run =
      let xmlns_prefix =
        {
          prefix = X.reserved_prefix;
          xmlns = X.forester_xmlns
        }
      in
      run ~reserved: [xmlns_prefix]
  end

  module Scope = Algaeff.Reader.Make(struct type t = iri option end)

  let transclusion_cache = Hashtbl.create 1000

  let forester_iri_to_string ~host ~path =
    if host = Params.host then
      String.concat "/" @@
        match path with
        | Iri.Absolute xs -> xs
        | Iri.Relative xs -> xs
    else
      Iri.to_string ~pctencode: false @@
        Iri.iri ~host ~path ()

  let iri_to_string iri =
    match Iri.host iri with
    | Some host when Iri.scheme iri = Iri_scheme.scheme ->
      let path = Iri.path iri in
      forester_iri_to_string ~host ~path
    | _ ->
      Iri.to_string ~pctencode: false iri

  let iri_type iri =
    match Iri.path iri with
    | Absolute ["unstable"; _] -> "machine"
    | Absolute ["hash"; _] -> "hash"
    | Absolute [_] -> "user"
    | _ -> failwith "addr_type"

  let home_iri =
    let@ root = Option.bind Params.home in
    let base = Iri_scheme.base_iri ~host: Params.host in
    try
      Option.some @@ Iri.resolve ~base @@ Iri.of_string root
    with
      | _ -> None

  let iri_is_home iri =
    match home_iri with
    | Some home_iri ->
      Iri.equal ~normalize: true home_iri iri
    | None -> false

  let route_resource_iri ~suffix iri =
    let host = Option.value ~default: "" @@ Iri.host iri in
    let bare_route =
      String.concat "-" @@
        match Iri.path iri with
        | Iri.Absolute xs -> xs
        | Iri.Relative xs -> xs (* impossible? *)
    in
    begin
      if host = Params.host then
        if iri_is_home iri then "index.xml"
        else
          bare_route ^ suffix
      else
        "foreign-" ^ host ^ "-" ^ bare_route ^ suffix
    end

  let route iri =
    match F.get_resource iri with
    | Some resource ->
      let suffix =
        match resource with
        | T.Article _ -> ".xml"
        | T.Asset _ -> ""
      in
      route_resource_iri ~suffix iri
    | None ->
      Iri.to_uri iri

  module PT = Plain_text_client.Make(F)(struct let route = route end)

  let get_expanded_title frontmatter =
    let scope = Scope.read () in
    let title = F.get_expanded_title ?scope ~flags: T.{ empty_when_untitled = true } frontmatter in
    T.apply_modifier_to_content Sentence_case title

  let render_xml_qname qname =
    let qname = Xmlns.normalise_qname qname in
    match qname.prefix with
    | "" -> qname.uname
    | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

  let render_xml_attr T.{ key; value } =
    let str_value = PT.string_of_content value in
    P.string_attr (render_xml_qname key) "%s" str_value

  let render_prim_node p =
    X.prim p []

  let render_img = function
    | T.Inline { format; base64 } ->
      X.img [X.src "data:image/%s;base64,%s" format base64]
    | T.Remote url ->
      X.img [X.src "%s" url]

  let render_xmlns_prefix Xmlns.{ prefix; xmlns } =
    P.string_attr ("xmlns:" ^ prefix) "%s" xmlns

  let render_section_flags (dict : T.section_flags) =
    [
      X.optional_ X.show_heading dict.header_shown;
      X.optional_ X.show_metadata dict.metadata_shown;
      X.optional_ X.hidden_when_empty dict.hidden_when_empty;
      X.optional_ X.expanded dict.expanded;
      X.optional_ X.toc dict.included_in_toc;
      X.optional_ X.numbered dict.numbered
    ]

  let rec render_section (section : T.content T.section) : P.node =
    let@ () = Xmlns.run in
    X.tree
      (render_section_flags section.flags)
      [
        render_frontmatter section.frontmatter;
        let@ () = Scope.run ~env: section.frontmatter.iri in
        X.mainmatter [] @@ render_content section.mainmatter
      ]

  and render_frontmatter (frontmatter : T.content T.frontmatter) : P.node =
    X.frontmatter
      []
      [
        render_attributions frontmatter.iri frontmatter.attributions;
        render_dates frontmatter.dates;
        X.optional (X.source_path [] "%s") frontmatter.source_path;
        X.anchor [] "%i" @@ Oo.id ( object end);
        X.optional (fun iri -> X.addr [X.type_ "%s" @@ iri_type iri] "%s" @@ iri_to_string iri) frontmatter.iri;
        X.optional (X.route [] "%s") @@ Option.map route frontmatter.iri;
        begin
          let title = get_expanded_title frontmatter in
          X.title [X.text_ "%s" @@ PT.string_of_content title] @@
            render_content title
        end;
        begin
          match frontmatter.taxon with
          | None -> X.null []
          | Some taxon ->
            X.taxon [] @@ render_content @@ T.apply_modifier_to_content T.Sentence_case taxon
        end;
        X.null (List.map render_meta frontmatter.metas)
      ]

  and render_meta (key, body) =
    X.meta [X.name "%s" key] @@
      render_content body

  and render_content (Content content: T.content) : P.node list =
    match content with
    | T.Text txt0 :: T.Text txt1 :: content ->
      render_content @@ Content (T.Text (txt0 ^ txt1) :: content)
    | node :: content ->
      let xs = render_content_node node in
      let ys = render_content (Content content) in
      xs @ ys
    | [] -> []

  and render_content_node : 'a T.content_node -> P.node list = function
    | Text str ->
      [P.txt "%s" str]
    | CDATA str ->
      [P.txt ~raw: true "<![CDATA[%s]]>" str]
    | Iri iri ->
      let relativised = Iri_scheme.relativise_iri ~host: Params.host iri in
      let str = Format.asprintf "%a" pp_iri relativised in
      [P.txt "%s" str]
    | Route_of_iri iri ->
      [P.txt "%s" (route iri)]
    | Xml_elt elt ->
      let prefixes_to_add, (name, attrs, content) =
        let@ () = Xmlns.within_scope in
        render_xml_qname elt.name,
        List.map render_xml_attr elt.attrs,
        render_content elt.content
      in
      let attrs =
        let xmlns_attrs = List.map render_xmlns_prefix prefixes_to_add in
        attrs @ xmlns_attrs
      in
      [P.std_tag name attrs content]
    | Prim (p, content) ->
      [render_prim_node p @@ render_content content]
    | Transclude transclusion ->
      render_transclusion transclusion
    | Contextual_number addr ->
      let custom_number =
        let@ article = Option.bind @@ F.get_article addr in
        article.frontmatter.number
      in
      begin
        match custom_number with
        | None -> [X.contextual_number [X.addr_ "%s" @@ iri_to_string addr]]
        | Some num -> [P.txt "%s" num]
      end
    | Link link ->
      render_link link
    | Results_of_query q ->
      let article_to_section =
        T.article_to_section
          ~flags: {
            T.default_section_flags with
            expanded = Some false;
            numbered = Some false;
            included_in_toc = Some false;
            metadata_shown = Some true
          }
      in
      F.run_query q
      |> Util.get_sorted_articles
      |> List.map (Fun.compose render_section article_to_section)
    | Results_of_datalog_query q ->
      let article_to_section =
        T.article_to_section
          ~flags: {
            T.default_section_flags with
            expanded = Some false;
            numbered = Some false;
            included_in_toc = Some false;
            metadata_shown = Some true
          }
      in
      F.run_datalog_query q
      |> Util.get_sorted_articles
      |> List.map (Fun.compose render_section article_to_section)
    | Section section ->
      [render_section section]
    | KaTeX (mode, content) ->
      let display =
        match mode with
        | Inline -> "inline"
        | Display -> "block"
      in
      let body = Format.asprintf "%a" T.TeX_like.pp_content content in
      [X.tex [X.display "%s" display] "<![CDATA[%s]]>" body]
    | TeX_cs cs ->
      (* Should not happen! *)
      [P.txt ~raw: true "\\%s" @@ TeX_cs.show cs]
    | Img img ->
      [render_img img]
    | Artefact resource ->
      [render_artefact resource]
    | Datalog_script _ -> []

  and render_artefact (resource : T.content T.artefact) =
    X.resource
      [X.hash "%s" resource.hash]
      [
        X.resource_content [] @@ render_content resource.content;
        render_resource_sources resource.sources
      ]

  and render_resource_sources sources =
    X.null @@ List.map render_resource_source sources

  and render_resource_source source =
    X.resource_source [X.type_ "%s" source.type_; X.resource_part "%s" source.part] "<![CDATA[%s]]>" source.source

  and render_transclusion (transclusion : T.content T.transclusion) : P.node list =
    match Hashtbl.find_opt transclusion_cache transclusion with
    | Some nodes -> nodes
    | None ->
      let content = F.get_content_of_transclusion transclusion in
      let nodes = render_content content in
      Hashtbl.add transclusion_cache transclusion nodes;
      nodes

  and render_link (link : T.content T.link) : P.node list =
    let article_opt = F.get_article link.href in
    let attrs =
      match article_opt with
      | None ->
        [
          X.href "%s" @@ route link.href;
          X.type_ "external"
        ]
      | Some article ->
        [
          X.optional_ (X.href "%s") @@ Option.map route article.frontmatter.iri;
          X.title_ "%s" @@ PT.string_of_content @@ get_expanded_title article.frontmatter;
          X.optional_ (X.addr_ "%s") @@ Option.map iri_to_string article.frontmatter.iri;
          X.type_ "local"
        ]
    in
    [X.link attrs @@ render_content link.content]

  and render_attributions =
    let attributions_cache = Hashtbl.create 1000 in
    fun scope attributions ->
      match Hashtbl.find_opt attributions_cache (scope, attributions) with
      | Some cached -> cached
      | None ->
        let result =
          match scope with
          | None -> X.null []
          | Some scope ->
            let indirect_attributions =
              let open Datalog_expr.Notation in
              let query =
                let positives = [Builtin_relation.has_indirect_contributor @* [const (T.Iri_vertex scope); var "X"]] in
                let negatives = [] in
                Datalog_expr.{ var = "X"; positives; negatives }
              in
              let@ biotree = List.filter_map @~ Util.get_sorted_articles @@ F.run_datalog_query query in
              let@ iri = Option.map @~ biotree.frontmatter.iri in
              T.{ vertex = T.Iri_vertex iri; role = Contributor }
            in
            let all_attributions =
              List_util.nub @@
              attributions @
              let@ attribution = List.filter_map @~ indirect_attributions in
              if List.exists (fun (existing : _ T.attribution) -> attribution.vertex = existing.vertex) attributions then None
              else
                Some attribution
            in
            X.authors [] @@ List.map render_attribution all_attributions
        in
        Hashtbl.add attributions_cache (scope, attributions) result;
        result

  and render_attribution attrib =
    let tag =
      match attrib.role with
      | Author -> X.author
      | Contributor -> X.contributor
    in
    tag [] @@ render_attribution_vertex attrib.vertex

  and render_attribution_vertex = function
    | T.Iri_vertex href ->
      let content = T.Content [T.Transclude { href; target = Title { empty_when_untitled = false }; modifier = Identity }] in
      render_link T.{ href; content }
    | T.Content_vertex content ->
      render_content content

  and render_dates dates =
    X.null @@ List.map render_date dates

  and render_date (date : Date.t) =
    let href_attr =
      let str = Format.asprintf "%a" Date.pp date in
      let base = Iri_scheme.base_iri ~host: Params.host in
      let iri = Iri.resolve ~base (Iri.of_string str) in
      match F.get_article iri with
      | None -> X.null_
      | Some _ -> X.href "%s" @@ route iri
    in
    X.date
      [href_attr]
      [
        X.year [] "%i" date.yyyy;
        date.mm |> X.optional @@ X.month [] "%i";
        date.dd |> X.optional @@ X.day [] "%i"
      ]

  let render_article (article : T.content T.article) : P.node =
    let xmlns_prefix = Xmlns.{ prefix = X.reserved_prefix; xmlns = X.forester_xmlns } in
    let@ () = Scope.run ~env: article.frontmatter.iri in
    let@ () = Xmlns.run in
    X.tree
      [
        render_xmlns_prefix xmlns_prefix;
        X.optional_ X.root @@ Option.map iri_is_home article.frontmatter.iri
      ]
      [
        render_frontmatter article.frontmatter;
        X.mainmatter [] @@ render_content article.mainmatter;
        X.backmatter [] @@ render_content article.backmatter
      ]

  let pp_xml ?stylesheet fmt article =
    Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
    Format.pp_print_newline fmt ();
    begin
      let@ uri = Option.iter @~ stylesheet in
      Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" uri
    end;
    Format.pp_print_newline fmt ();
    P.pp_xml fmt @@ render_article article
end
