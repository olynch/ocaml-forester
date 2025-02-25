(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_xml_names
open Forester_core
open Forester_compiler

module T = Types
module P = Pure_html
module X = Xml_forester

module Xmlns = struct
  include Xmlns_effect.Make ()

  let run =
    let xmlns_prefix = {
      prefix = X.reserved_prefix;
      xmlns = X.forester_xmlns
    }
    in
    run ~reserved: [xmlns_prefix]
end

module Scope = Algaeff.Reader.Make(struct type t = iri option end)
module Loop_detection = Algaeff.Reader.Make(struct type t = Iri_set.t end)

(* It's fine to have a global transclusion cache since iris fully qualify a tree*)
let transclusion_cache = Hashtbl.create 1000

(* This would be nice, but it is interfering with the stupid breadcrumb titles! Need to make that stuff stateless. *)
(* let frontmatter_cache = Hashtbl.create 1000 *)

let forester_iri_to_string ~host ~path ~(config : Config.t) =
  if host = config.host then
    String.concat "/" @@
      match path with
      | Iri.Absolute xs -> xs
      | Iri.Relative xs -> xs
  else
    Iri.to_string ~pctencode: false @@
      Iri.iri ~host ~path ()

let iri_to_string ~config iri =
  match Iri.host iri with
  | Some host when Iri.scheme iri = Iri_scheme.scheme ->
    let path = Iri.path iri in
    forester_iri_to_string ~host ~path ~config
  | _ ->
    Iri.to_string ~pctencode: false iri

let home_iri ~(config : Config.t) =
  (* let config = State.get_config forest in *)
  let@ root = Option.bind config.home in
  let base = Iri_scheme.base_iri ~host: config.host in
  try
    Option.some @@ Iri.resolve ~base @@ Iri.of_string root
  with
    | _ -> None

let iri_is_home ~config iri =
  match home_iri ~config with
  | Some home_iri ->
    (* By this point, any IRI should be in normal form. *)
    Iri.equal ~normalize: false home_iri iri
  | None -> false

let route_resource_iri ~suffix forest iri =
  let config = State.config forest in
  let host = Option.value ~default: "" @@ Iri.host iri in
  let bare_route =
    String.concat "-" @@
      match Iri.path iri with
      | Iri.Absolute xs -> xs
      | Iri.Relative xs -> xs (* impossible? *)
  in
  begin
    if host = config.host then
      if iri_is_home ~config iri then "index.xml"
      else
        bare_route ^ suffix
    else
      "foreign-" ^ host ^ "-" ^ bare_route ^ suffix
  end

let route forest iri =
  match Forest.find_opt (State.resources forest) iri with
  | Some resource ->
    let suffix =
      match resource with
      | T.Article _ -> ".xml"
      | T.Asset _ -> ""
    in
    route_resource_iri ~suffix forest iri
  | None when Iri.scheme iri = Iri_scheme.scheme ->
    Reporter.emitf Broken_link "Could not route link to resource %a" pp_iri iri;
    Iri.to_uri iri
  | None ->
    Iri.to_uri iri

let get_sorted_articles (forest : State.t) addrs =
  let module C = Types.Comparators(struct
    let string_of_content =
      Plain_text_client.string_of_content
        ~forest: forest.resources
        ~router: (route forest)
  end) in
  addrs
  |> Vertex_set.to_seq
  |> Seq.filter_map Vertex.iri_of_vertex
  |> Seq.filter_map (fun iri -> Forest.get_article iri forest.resources)
  |> List.of_seq
  |> List.sort C.compare_article

let get_expanded_title frontmatter forest =
  let scope = Scope.read () in
  let title = Forest.get_expanded_title ?scope ~flags: T.{empty_when_untitled = true} frontmatter forest in
  T.apply_modifier_to_content Sentence_case title

let render_xml_qname qname =
  let qname = Xmlns.normalise_qname qname in
  match qname.prefix with
  | "" -> qname.uname
  | _ -> Format.sprintf "%s:%s" qname.prefix qname.uname

let render_xml_attr (forest : State.t) T.{key; value} =
  let str_value = Plain_text_client.string_of_content ~forest: forest.resources ~router: (route forest) value in
  P.string_attr (render_xml_qname key) "%s" str_value

let render_prim_node p =
  X.prim p []

let render_img = function
  | T.Inline {format; base64} ->
    X.img [X.src "data:image/%s;base64,%s" format base64]
  | T.Remote url ->
    X.img [X.src "%s" url]

let render_xmlns_prefix Xmlns.{prefix; xmlns} =
  P.string_attr ("xmlns:" ^ prefix) "%s" xmlns

let render_section_flags (dict : T.section_flags) = [
  X.optional_ X.show_heading dict.header_shown;
  X.optional_ X.show_metadata dict.metadata_shown;
  X.optional_ X.hidden_when_empty dict.hidden_when_empty;
  X.optional_ X.expanded dict.expanded;
  X.optional_ X.toc dict.included_in_toc;
  X.optional_ X.numbered dict.numbered
]

let add_seen_iri iri kont =
  let@ () = Loop_detection.scope @@ Iri_set.add iri in
  kont ()

let add_seen_iri_opt iri_opt kont =
  match iri_opt with
  | None -> kont ()
  | Some iri -> add_seen_iri iri kont

let have_seen_iri iri =
  Iri_set.mem iri @@ Loop_detection.read ()

let have_seen_iri_opt iri_opt =
  match iri_opt with
  | None -> false
  | Some iri -> have_seen_iri iri

let rec render_section forest (section : T.content T.section) : P.node =
  let@ () = Xmlns.run in
  X.tree
    (render_section_flags section.flags)
    [
      render_frontmatter forest section.frontmatter;
      let@ () = Scope.run ~env: section.frontmatter.iri in
      X.mainmatter [] @@
        if have_seen_iri_opt section.frontmatter.iri then
          [X.info [] [P.txt "Transclusion loop detected, rendering stopped."]]
        else
          let@ () = add_seen_iri_opt section.frontmatter.iri in
          render_content forest section.mainmatter
    ]

and render_frontmatter forest (frontmatter : T.content T.frontmatter) : P.node =
  (* match Hashtbl.find_opt frontmatter_cache frontmatter with *)
  (* | Some cached -> cached *)
  (* | None -> *)
  let config = State.config forest in
  let result =
    X.frontmatter
      []
      [
        render_attributions forest frontmatter.iri frontmatter.attributions;
        render_dates forest frontmatter.dates;
        X.conditional forest.dev (X.optional (X.source_path [] "%s") frontmatter.source_path);
        X.optional (fun iri -> X.addr [] "%s" @@ iri_to_string ~config iri) frontmatter.iri;
        X.optional (X.route [] "%s") @@ Option.map (route forest) frontmatter.iri;
        begin
          let title = get_expanded_title frontmatter forest.resources in
          X.title [X.text_ "%s" @@ Plain_text_client.string_of_content ~forest: forest.resources ~router: (route forest) title] @@
            render_content forest title
        end;
        begin
          match frontmatter.taxon with
          | None -> X.null []
          | Some taxon ->
            X.taxon [] @@ render_content forest (T.apply_modifier_to_content T.Sentence_case taxon)
        end;
        X.null (List.map (render_meta forest) frontmatter.metas)
      ]
  in
  (* Hashtbl.add frontmatter_cache frontmatter result; *)
  result

and render_meta forest (key, body) =
  X.meta [X.name "%s" key] @@
    render_content forest body

and render_content (forest : State.t) (Content content: T.content) : P.node list =
  match content with
  | T.Text txt0 :: T.Text txt1 :: content ->
    render_content forest (Content (T.Text (txt0 ^ txt1) :: content))
  | node :: content ->
    let xs = render_content_node forest node in
    let ys = render_content forest (Content content) in
    xs @ ys
  | [] -> []

and render_content_node
  : State.t -> 'a T.content_node -> P.node list
= fun forest node ->
  let config = State.config forest in
  match node with
  | Text str ->
    [P.txt "%s" str]
  | CDATA str ->
    [P.txt ~raw: true "<![CDATA[%s]]>" str]
  | Iri iri ->
    let relativised = Iri_scheme.relativise_iri ~host: config.host iri in
    let str = Format.asprintf "%a" pp_iri relativised in
    [P.txt "%s" str]
  | Route_of_iri iri ->
    [P.txt "%s" (route forest iri)]
  | Xml_elt elt ->
    let prefixes_to_add, (name, attrs, content) =
      let@ () = Xmlns.within_scope in
      render_xml_qname elt.name,
      List.map (render_xml_attr forest) elt.attrs,
      render_content forest elt.content
    in
    let attrs =
      let xmlns_attrs = List.map render_xmlns_prefix prefixes_to_add in
      attrs @ xmlns_attrs
    in
    [P.std_tag name attrs content]
  | Prim (p, content) ->
    [render_prim_node p @@ render_content forest content]
  | Transclude transclusion ->
    render_transclusion forest transclusion
  | Contextual_number addr ->
    let custom_number =
      let@ resource = Option.bind @@ Forest.find_opt forest.resources addr in
      match resource with
      | T.Article article ->
        article.frontmatter.number
      | T.Asset _ -> None
    in
    begin
      match custom_number with
      | None -> [X.contextual_number [X.addr_ "%s" @@ iri_to_string ~config addr]]
      | Some num -> [P.txt "%s" num]
    end
  | Link link ->
    render_link forest link
  | Results_of_query q ->
    let article_to_section =
      T.article_to_section
        ~flags: {T.default_section_flags with
          expanded = Some false;
          numbered = Some false;
          included_in_toc = Some false;
          metadata_shown = Some true
        }
    in
    let module Legacy_query_engine = (val (Forest.legacy_query_engine (State.graphs forest))) in
    Legacy_query_engine.run_query q
    |> get_sorted_articles forest
    |> List.map article_to_section
    |> List.map (render_section forest)
  | Results_of_datalog_query q ->
    let article_to_section =
      T.article_to_section
        ~flags: {T.default_section_flags with
          expanded = Some false;
          numbered = Some false;
          included_in_toc = Some false;
          metadata_shown = Some true
        }
    in
    Forest.run_datalog_query (State.graphs forest) q
    |> get_sorted_articles forest
    |> List.map article_to_section
    |> List.map (render_section forest)
  | Section section ->
    [render_section forest section]
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
    (* assert false *)
    [P.txt ~raw: true "\\%s" @@ TeX_cs.show cs]
  | Img img ->
    [render_img img]
  | Artefact resource ->
    [render_artefact forest resource]
  | Datalog_script _ -> []

and render_artefact forest (resource : T.content T.artefact) =
  X.resource
    [X.hash "%s" resource.hash]
    [
      X.resource_content [] @@ render_content forest resource.content;
      render_resource_sources resource.sources
    ]

and render_resource_sources sources =
  X.null @@ List.map render_resource_source sources

and render_resource_source source =
  X.resource_source [X.type_ "%s" source.type_; X.resource_part "%s" source.part] "<![CDATA[%s]]>" source.source

and render_transclusion (forest : State.t) (transclusion : T.content T.transclusion) : P.node list =
  match Hashtbl.find_opt transclusion_cache transclusion with
  | Some nodes -> nodes
  | None ->
    match Forest.get_content_of_transclusion transclusion forest.resources with
    | None ->
      Reporter.fatalf Resource_not_found "Could not find tree %a" pp_iri transclusion.href
    | Some content ->
      let nodes = render_content forest content in
      Hashtbl.add transclusion_cache transclusion nodes;
      nodes

and render_link (forest : State.t) (link : T.content T.link) : P.node list =
  let config = State.config forest in
  let article_opt = Forest.get_article link.href forest.resources in
  let attrs =
    match article_opt with
    | None ->
      [
        X.href "%s" @@ route forest link.href;
        X.type_ "external"
      ]
    | Some article ->
      [
        X.optional_ (X.href "%s") @@ Option.map (route forest) article.frontmatter.iri;
        X.title_ "%s" @@
        Plain_text_client.string_of_content ~forest: forest.resources ~router: (route forest) @@
        get_expanded_title article.frontmatter forest.resources;
        X.optional_ (X.addr_ "%s") @@ Option.map (iri_to_string ~config) article.frontmatter.iri;
        X.type_ "local"
      ]
  in
  [X.link attrs @@ render_content forest link.content]

(* Note: this is not a big bottleneck. *)
and render_attributions =
  let attributions_cache = Hashtbl.create 1000 in
  fun (forest : State.t) scope (attributions : _ T.attribution list) ->
    match Hashtbl.find_opt attributions_cache (scope, attributions) with
    | Some cached -> cached
    | None ->
      let result =
        match scope with
        | None -> X.null []
        | Some scope ->
          let indirect_attributions =
            let open Datalog_expr.Notation in
            let articles =
              let positives = [Builtin_relation.has_indirect_contributor @* [const (T.Iri_vertex scope); var "X"]] in
              let negatives = [] in
              Datalog_expr.{var = "X"; positives; negatives}
              |> Forest.run_datalog_query (State.graphs forest)
              |> get_sorted_articles forest
            in
            let@ biotree = List.filter_map @~ articles in
            let@ iri = Option.map @~ biotree.frontmatter.iri in
            T.{vertex = T.Iri_vertex iri; role = Contributor}
          in
          let all_attributions =
            attributions @
              let@ attribution = List.filter_map @~ indirect_attributions in
              if List.exists (fun (existing : _ T.attribution) -> Vertex.equal attribution.vertex existing.vertex) attributions then None
              else
                Some attribution
          in
          X.authors [] @@ List.map (render_attribution forest) all_attributions
      in
      Hashtbl.add attributions_cache (scope, attributions) result;
      result

and render_attribution forest (attrib : _ T.attribution) =
  let tag =
    match attrib.role with
    | Author -> X.author
    | Contributor -> X.contributor
  in
  tag [] @@ render_attribution_vertex forest attrib.vertex

and render_attribution_vertex (forest : State.t) vtx =
  match vtx with
  | T.Iri_vertex href ->
    let content = T.Content [T.Transclude {href; target = Title {empty_when_untitled = false}; modifier = Identity}] in
    render_link forest T.{href; content}
  | T.Content_vertex content ->
    render_content forest content

and render_dates forest dates =
  X.null @@ List.map (render_date forest) dates

and render_date forest (date : Human_datetime.t) =
  let config = State.config forest in
  let href_attr =
    let str = Format.asprintf "%a" Human_datetime.pp (Human_datetime.drop_time date) in
    let base = Iri_scheme.base_iri ~host: config.host in
    let iri = Iri.resolve ~base (Iri.of_string str) in
    match Forest.get_article iri forest.resources with
    | None -> X.null_
    | Some _ -> X.href "%s" @@ route forest iri
  in
  X.date
    [href_attr]
    [
      X.year [] "%i" (Human_datetime.year date);
      Human_datetime.month date |> X.optional @@ X.month [] "%i";
      Human_datetime.day date |> X.optional @@ X.day [] "%i"
    ]

let render_article forest (article : T.content T.article) : P.node =
  let@ () = Reporter.tracef "when rendering article %a" Format.(pp_print_option Iri.pp) article.frontmatter.iri in
  let config = State.config forest in
  let xmlns_prefix = Xmlns.{prefix = X.reserved_prefix; xmlns = X.forester_xmlns} in
  let@ () = Loop_detection.run ~env: Iri_set.empty in
  let@ () = Scope.run ~env: article.frontmatter.iri in
  let@ () = Xmlns.run in
  X.tree
    [
      render_xmlns_prefix xmlns_prefix;
      X.optional_ X.root @@ Option.map (iri_is_home ~config) article.frontmatter.iri
    ]
    [
      render_frontmatter forest article.frontmatter;
      X.mainmatter [] @@
        begin
          let@ () = add_seen_iri_opt article.frontmatter.iri in
          render_content forest article.mainmatter
        end;
      X.backmatter [] @@ render_content forest article.backmatter
    ]

(* let pp_xml ?stylesheet fmt article = *)
(*   Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|}; *)
(*   Format.pp_print_newline fmt (); *)
(*   begin *)
(*     let@ uri = Option.iter @~ stylesheet in *)
(*     Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" uri *)
(*   end; *)
(*   Format.pp_print_newline fmt (); *)
(*   P.pp_xml fmt @@ render_article article *)
