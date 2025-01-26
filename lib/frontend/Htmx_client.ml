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
module H = P.HTML

type query = {
  query: (string, T.content T.vertex) Forester_core.Datalog_expr.query;
}
[@@deriving repr]

module Xmlns = Xmlns_effect.Make ()

let get_sorted_articles (forest : State.t) addrs =
  let module C = Types.Comparators(struct
    let string_of_content =
      Plain_text_client.string_of_content
        ~forest: forest.resources
        ~router: Iri.to_uri (* TODO *)
  end) in
  addrs
  |> Vertex_set.to_seq
  |> Seq.filter_map Vertex.iri_of_vertex
  |> Seq.filter_map (fun iri -> Forest.get_article iri forest.resources)
  |> List.of_seq
  |> List.sort C.compare_article

let route forest addr =
  let config = State.config forest in
  if Some addr = Option.map (Iri_scheme.user_iri ~host: config.host) config.home then
    "index.html"
  else
    Format.asprintf "%s" (Iri.path_string addr)

let render_xml_qname = function
  | { prefix = ""; uname; _ } -> uname
  | { prefix; uname; _ } -> Format.sprintf "%s:%s" prefix uname

let render_xml_attr
    : T.content T.xml_attr -> _
  = fun T.{ key; value = _ } ->
    P.string_attr (render_xml_qname key) "todo"
(* "%a" render_content value *)

let tag_of_prim_node : Prim.t -> P.attr list -> P.node list -> P.node = function
  | `P -> H.p
  | `Em -> H.em
  | `Strong -> H.strong
  | `Figure -> H.figure
  | `Figcaption -> H.figcaption
  | `Ul -> H.ul
  | `Ol -> H.ol
  | `Li -> H.li
  | `Blockquote -> H.blockquote
  | `Code -> H.code
  | `Pre -> H.pre

let render_prim_node p =
  tag_of_prim_node p []

let render_img = function
  | T.Inline { format; base64 } ->
    H.img [H.src "data:image/%s;base64,%s" format base64]
  | T.Remote url ->
    H.img [H.src "%s" url]

let render_xmlns_prefix Xmlns.{ prefix; xmlns } =
  P.string_attr ("xmlns:" ^ prefix) "%s" xmlns

let rec render_article (forest : State.t) (article : T.content T.article) : P.node =
  (* FIXME: What should reserved be here? *)
  let@ () = Xmlns.run ~reserved: [] in
  H.html
    []
    [
      H.head
        []
        [
          H.meta [H.charset "utf-8"];
          (* H.link [H.rel "stylesheet"; H.href "style.css"]; *)
          H.link
            [
              H.rel "stylesheet";
              H.href "https://cdn.jsdelivr.net/npm/katex@0.16.20/dist/katex.min.css";
              H.integrity "sha384-sMefv1J1YJCHsg0mTa9YG+n/9KnJb9lGrJUUY5arg6bAL1qps/oZjmUwaHlX5Ugg";
              H.crossorigin `anonymous
            ];
          H.script
            [
              H.defer;
              H.src "https://cdn.jsdelivr.net/npm/katex@0.16.20/dist/katex.min.js";
              H.integrity "sha384-i9p+YmlwbK0lT9RcfgdAo/Cikui1KeFMeV/0Fwsu+rzgsCvas6oUptNOmo29C33p";
              H.crossorigin `anonymous
            ]
            "";
          H.script
            [
              H.defer;
              H.src "https://cdn.jsdelivr.net/npm/katex@0.16.20/dist/contrib/auto-render.min.js";
              H.integrity "sha384-hCXGrW6PitJEwbkoStFjeJxv+fSOOQKOPbJxSfM6G5sWZjAyWhXiTIIAmQqnlLlh";
              H.crossorigin `anonymous;
              P.string_attr "onload" "renderMathInElement(document.body);"
            ]
            "";
          H.script [H.src "https://unpkg.com/htmx.org@2.0.4/dist/htmx.js"] "";
          H.script [H.src "https://unpkg.com/htmx.org/dist/ext/json-enc.js"] "";
          H.script
            [
            ]
            {js|
              document.body.addEventListener('htmx:load', function(evt) {
                myJavascriptLib.init(evt.detail.elt);
              });
            |js};
        ];
      H.body
        []
        [
          H.article
            []
            [
              render_frontmatter forest article.frontmatter;
              H.null @@ render_content forest article.mainmatter;
              H.section [H.class_ "backmatter"] @@ render_content forest article.backmatter
            ]
        ]
    ]

and render_section (forest : State.t) (section : T.content T.section) : P.node =
  H.section
    []
    [
      render_frontmatter forest section.frontmatter;
      H.null @@ render_content forest section.mainmatter
    ]

and render_frontmatter (forest : State.t) (frontmatter : T.content T.frontmatter) : P.node =
  H.header
    []
    [
      H.h1 [] @@
      List.concat @@
      Option.to_list @@
      Option.map
        (render_content forest)
        frontmatter.title
    ]

and render_content (forest : State.t) (Content content: T.content) : P.node list =
  List.concat_map (render_content_node forest) content

and render_content_node
    : State.t -> 'a T.content_node -> P.node list
  = fun forest node ->
    let open P in
    (* let open H in *)
    match node with
    | Text str ->
      [P.txt "%s" str]
    | CDATA str ->
      [P.txt ~raw: true "<![CDATA[%s]]>" str]
    | Xml_elt elt ->
      let prefixes_to_add, (name, attrs, content) =
        let@ () = Xmlns.within_scope in
        render_xml_qname elt.name,
        List.map render_xml_attr elt.attrs,
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
        let@ article = Option.bind @@ Forest.get_article addr forest.resources in
        article.frontmatter.number
      in
      let num =
        match custom_number with
        | None -> Format.asprintf "[%a]" Iri.pp addr
        | Some num -> num
      in
      [P.txt "%s" num]
    | Link link ->
      render_link forest link
    | Results_of_query q ->
      let module Graphs = (val forest.graphs) in
      let module Engine = Legacy_query_engine.Make(Graphs) in
      Engine.run_query q
      |> get_sorted_articles forest
      |> List.map (Fun.compose (render_section forest) T.article_to_section)
    | Section section ->
      [render_section forest section]
    | KaTeX (mode, content) ->
      let l, r =
        match mode with
        | Display -> {|\[|}, {|\]|}
        | Inline -> {|\(|}, {|\)|}
      in
      let body = Plain_text_client.string_of_content ~forest: forest.resources ~router: Iri.to_uri content in
      [P.txt ~raw: true "%s%s%s" l body r]
    | TeX_cs cs ->
      [P.txt ~raw: true "\\%s" (TeX_cs.show cs)]
    | Img img ->
      [render_img img]
    | T.Results_of_datalog_query q ->
      (* We could just evaluate the query immediately. This is just experimental*)
      [
        H.div
          []
          [
            H.div
              [
                Hx.get "/query";
                Hx.trigger "load";
                (* Hx.headers {|{"Content-Type": "application/json"}|}; *)
                (* Hx.ext "json-enc"; *)
                Hx.vals
                  "%s"
                  Repr.(
                    to_json_string
                      ~minify: true
                      (* Datalog_expr.(query_t Repr.string (T.vertex_t T.content_t)) *)
                      query_t
                      { query = q }
                  )
              ]
              []
          ]
      ]
    | T.Datalog_script _ -> []
    | T.Artefact _
    | T.Iri _
    | T.Route_of_iri _ ->
      [P.txt "todo"]

and _render_resource resource =
  render_content resource.contents

and render_transclusion (forest : State.t) (transclusion : T.content T.transclusion) : P.node list =
  List.concat @@
  Option.to_list @@
  Option.map (render_content forest) @@
  Forest.get_content_of_transclusion transclusion forest.resources

(* TODO: links need to be flattened in order to produce valid HTML. *)
and render_link (forest : State.t) (link : T.content T.link) : P.node list =
  let article_opt = Forest.get_article link.href forest.resources in
  let attrs =
    match article_opt with
    | None ->
      [H.href "%s" (Format.asprintf "%a" Iri.pp link.href)]
    | Some article ->
      [
        begin
          match article.frontmatter.iri with
          | Some iri -> H.href "%s" @@ route forest iri
          | None -> P.HTML.null_
        end;
        (* H.title_ "%s" @@ PT.string_of_content article.frontmatter.title *)
      ]
  in
  [H.a attrs @@ render_content forest link.content]

let render_query_result (forest : State.t) vs =
  let render_vertex = function
    | T.Iri_vertex iri ->
      begin
        match Forest.find_opt forest.resources iri with
        | None ->
          H.li
            []
            [
              H.a
                [H.href "%s" (Format.asprintf "%a" pp_iri iri)]
                [P.txt "%s" (Format.asprintf "%a" pp_iri iri)]
            ]
        | Some (T.Article a) ->
          H.li
            []
            [
              render_frontmatter forest a.frontmatter;
              H.div [] @@ render_content forest a.mainmatter
            ]
        | Some (T.Asset _) ->
          P.txt "todo: render asset"
      end
    | T.Content_vertex c -> H.div [] (render_content forest c)
  in
  vs
  |> Vertex_set.to_list (* TODO: Needs to be sorted *)
  |> List.map render_vertex
