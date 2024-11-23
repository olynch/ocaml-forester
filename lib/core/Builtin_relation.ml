(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

let make_builtin name = "org.forester.rel." ^ name

let links_to = make_builtin "links-to"
let transcludes = make_builtin "transcludes"
let has_author = make_builtin "authored-by"
let has_taxon = make_builtin "has-taxon"
let has_tag = make_builtin "has-tag"
let is_node = make_builtin "is-node"
let in_bundle_closure = make_builtin "in-bundle-closure"
let in_bundle_step = make_builtin "in-bundle-step"
let in_host = make_builtin "in-host"

let transcludes_rtc = make_builtin "transcludes.reflexive-transitive-closure"
let transcludes_tc = make_builtin "transcludes.transitive-closure"
let references = make_builtin "references"
let is_reference = make_builtin "is-reference"
let is_person = make_builtin "is-person"

let has_direct_contributor = make_builtin "has-direct-contributor"
let has_indirect_contributor = make_builtin "has-indirect-contributor"

module Dx = Datalog_expr
open Dx.Notation

let reference_taxon : Vertex.t Dx.term =
  Const (Content_vertex (Content [Text "reference"]))

let person_taxon : Vertex.t Dx.term =
  Const (Content_vertex (Content [Text "person_taxon"]))

let axioms : _ Dx.script =
  [
    is_reference @* [var "X"] << [has_taxon @* [var "X"; reference_taxon]];
    is_person @* [var "X"] << [has_taxon @* [var "X"; person_taxon]];
    transcludes_tc @* [var "X"; var "Y"] << [transcludes @* [var "X"; var "Y"]];
    transcludes_tc @* [var "X"; var "Z"] << [transcludes_tc @* [var "X"; var "Y"]; transcludes @* [var "Y"; var "Z"]];
    transcludes_rtc @* [var "X"; var "X"] << [is_node @* [var "X"]];
    transcludes_rtc @* [var "X"; var "Y"] << [transcludes_tc @* [var "X"; var "Y"]];
    references @* [var "X"; var "Z"]
    << [
      transcludes_rtc @* [var "X"; var "Y"];
      links_to @* [var "Y"; var "Z"];
      is_reference @* [var "Z"]
    ];
    has_direct_contributor @* [var "X"; var "Y"] << [has_author @* [var "X"; var "Y"]];
    has_indirect_contributor @* [var "X"; var "Z"] << [transcludes_rtc @* [var "X"; var "Y"]; has_direct_contributor @* [var "Y"; var "Z"]];
    in_bundle_closure @* [var "X"; var "X"] << [is_node @* [var "X"]];
    in_bundle_closure @* [var "X"; var "Z"] << [in_bundle_closure @* [var "X"; var "Y"]; in_bundle_step @* [var "Y"; var "Z"]];
    in_bundle_step @* [var "X"; var "Y"] << [transcludes @* [var "X"; var "Y"]];
  ]
