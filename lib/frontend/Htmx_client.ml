(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_xml_names
open Forester_core

module T = Types
module P = Pure_html
module H = P.HTML

module type S = sig
  val route : iri -> string
  val render_article : Compiler.state -> T.content T.article -> P.node
end

module type Params = sig
  val forest : Compiler.state
end

module Make
  (Params: Params)
  ()
  (* : S =  *)
= struct

  module PT = Plain_text_client.Make(struct let route = Iri.to_uri let forest = Params.forest end)
  module Xmlns = Xmlns_effect.Make ()
  let forest = Params.forest

  let route addr =
    let config = Compiler.get_config forest in
    if Some addr = (Option.map (Iri_scheme.user_iri ~host: config.host) config.home) then
      "index.html"
    else
      Format.asprintf "%a.html" Iri.pp addr

  let render_xml_qname = function
    | { prefix = ""; uname; _ } -> uname
    | { prefix; uname; _ } -> Format.sprintf "%s:%s" prefix uname

  let render_xml_attr
      : T.content T.xml_attr -> _
    = fun
        T.{ key; value = _ }
      ->
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

  let rec render_article (forest : Compiler.state) (article : T.content T.article) : P.node =
    H.html
      []
      [
        H.head
          []
          [
            H.meta [H.charset "utf-8"];
            H.link [H.rel "stylesheet"; H.href "style.css"]
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

  and render_section (forest : Compiler.state) (section : T.content T.section) : P.node =
    H.section
      []
      [
        render_frontmatter forest section.frontmatter;
        H.null @@ render_content forest section.mainmatter
      ]

  and render_frontmatter (forest : Compiler.state) (frontmatter : T.content T.frontmatter) : P.node =
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

  and render_content (forest : Compiler.state) (Content content: T.content) : P.node list =
    List.concat_map (render_content_node forest) content

  and render_content_node : Compiler.state -> 'a T.content_node -> P.node list = fun forest node ->
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
          let@ article = Option.bind @@ Compiler.get_article addr forest in
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
      | Results_of_query _q ->
        (* Forest.run_query q *)
        (* |> Util.get_sorted_articles *)
        (* |> List.map (Fun.compose render_section T.article_to_section) *)
        assert false
      | Section section ->
        [render_section forest section]
      | KaTeX (mode, content) ->
        let l, r =
          match mode with
          | Display -> {|\[|}, {|\]|}
          | Inline -> {|\(|}, {|\)|}
        in
        let body = PT.string_of_content content in
        [P.txt ~raw: true "%s%s%s" l body r]
      | TeX_cs cs ->
        [P.txt ~raw: true "\\%s" (TeX_cs.show cs)]
      | Img img ->
        [render_img img]
      | _ -> [P.txt "todo"]
  (* | Resource resource -> *)
  (*   render_resource resource *)

  and _render_resource resource =
    render_content resource.contents

  and render_transclusion (forest : Compiler.state) (transclusion : T.content T.transclusion) : P.node list =
    List.concat @@
    Option.to_list @@
    Option.map (render_content forest) @@
    Compiler.get_content_of_transclusion transclusion forest

  (* TODO: links need to be flattened in order to produce valid HTML. *)
  and render_link (forest : Compiler.state) (link : T.content T.link) : P.node list =
    let article_opt = Compiler.get_article link.href forest in
    let attrs =
      match article_opt with
      | None ->
        [H.href "%s" (Format.asprintf "%a" Iri.pp link.href)]
      | Some article ->
        [
          (
            match article.frontmatter.iri with
            | Some iri ->
              (H.href "%s" @@ route iri)
            | None -> P.HTML.null_
          );
          (* H.title_ "%s" @@ PT.string_of_content article.frontmatter.title *)
        ]
    in
    [H.a attrs @@ render_content forest link.content]
end
