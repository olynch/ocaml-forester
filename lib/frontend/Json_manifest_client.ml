(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_compiler
open Forester_core

module T = Types

module PT = Plain_text_client

let render_tree ~dev ~forest (doc : T.content T.article) : (string * Yojson.Safe.t) option =
  let host = (State.config forest).host in
  let@ iri = Option.bind doc.frontmatter.iri in
  (* TODO : Check routing *)
  let route = Legacy_xml_client.route forest iri in
  let title_string =
    String_util.sentence_case @@
    PT.string_of_content forest.resources @@
    Forest.get_expanded_title doc.frontmatter forest.resources
  in
  let title = `String title_string in
  let taxon =
    match doc.frontmatter.taxon with
    | None -> `Null
    | Some vertex ->
      let content = T.apply_modifier_to_content Sentence_case vertex in
      `String (PT.string_of_content forest.resources content)
  in
  let tags =
    `List
      begin
        let@ tag = List.filter_map @~ doc.frontmatter.tags in
        let@ content = Option.map @~ Forest.get_title_or_content_of_vertex ~modifier: Identity tag forest.resources in
        `String (PT.string_of_content forest.resources content)
      end
  in
  let route = `String route in
  let metas =
    let meta_string meta = String.trim @@ PT.string_of_content forest.resources meta in
    let meta_assoc (s, meta) = (s, `String (meta_string meta)) in
    `Assoc (List.map meta_assoc doc.frontmatter.metas)
  in
  let path =
    if dev then
      match doc.frontmatter.source_path with
      | Some p -> [("sourcePath", `String p)]
      | None -> []
    else []
  in
  (* TODO: filter out anonymous stuff *)
  Option.some @@
    let fm =
      path @
        [
          ("title", title);
          ("taxon", taxon);
          ("tags", tags);
          ("route", route);
          ("metas", metas)
        ]
    in
    (Iri.to_string ~pctencode: false (Iri_scheme.relativise_iri ~host iri), `Assoc fm)

let render_trees
    ~(dev : bool)
    ~(forest : State.t)
    : Yojson.Safe.t
  =
  let trees = Forest.get_all_articles forest.resources in
  `Assoc (List.filter_map (render_tree ~dev ~forest) trees)
