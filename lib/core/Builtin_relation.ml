let make_builtin name = "org.forester.rel." ^ name

let links = make_builtin "links"
let transclusion = make_builtin "transclusion"
let authors = make_builtin "authors"
let contributors = make_builtin "contributors"
let taxa = make_builtin "taxa"
let tags = make_builtin "tags"
let is_node = make_builtin "nodes"

let transclusion_rtc = make_builtin "transclusion.rtc"
let transclusion_tc = make_builtin "transclusion.tc"
let references = make_builtin "references"
let is_reference = make_builtin "is-reference"
let is_person = make_builtin "is-person"

let contribution = make_builtin "contribution"

module Dx = Datalog_expr
open Dx.Notation

let reference_taxon : Vertex.t Dx.term =
  Const (Content_vertex (Content [Text "reference"]))

let person_taxon : Vertex.t Dx.term =
  Const (Content_vertex (Content [Text "person_taxon"]))

let axioms : _ Dx.script =
  [
    is_reference @* [var "X"] << [taxa @* [var "X"; reference_taxon]];
    is_person @* [var "X"] << [taxa @* [var "X"; person_taxon]];
    transclusion_tc @* [var "X"; var "Y"] << [transclusion @* [var "X"; var "Y"]];
    transclusion_tc @* [var "X"; var "Z"] << [transclusion_tc @* [var "X"; var "Y"]; transclusion @* [var "Y"; var "Z"]];
    transclusion_rtc @* [var "X"; var "X"] << [is_node @* [var "X"]];
    transclusion_rtc @* [var "X"; var "Y"] << [transclusion_tc @* [var "X"; var "Y"]];
    references @* [var "X"; var "Z"]
    << [
      transclusion_rtc @* [var "X"; var "Y"];
      links @* [var "Y"; var "Z"];
      is_reference @* [var "Z"]
    ];
    contribution @* [var "X"; var "Y"] << [authors @* [var "X"; var "Y"]];
    contribution @* [var "X"; var "Y"] << [contributors @* [var "X"; var "Y"]]
  ]
