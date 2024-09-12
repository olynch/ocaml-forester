open Forester_frontend.DSL
open Forester_core
open Xml_tree
open Query

let content =
  p
    [
      ol
        [
          li [txt "First item"];
          li [txt "Second item"]
        ];

      em [txt "Emphasized item"];
      strong [txt "Strong text"];
      code [txt "fun _ -> ()"];
      blockquote [txt "blockquote"];
      pre [txt "pre"];
      figure [txt "figure"];
      figcaption [txt "caption"];
      cdata "cdata";
      xml_elt "html" [];
      transclude "foo-001";
      contextual_number "chapter-3";
      results_of_query (union []);
      katex Inline (Content [txt "a = b"]);
      tex "\\begin{}";
      link "https://git.sr.ht/~jonsterling/ocaml-forester" [txt "Forester"];
      img "img.png";
      resource [txt "res"];
    ]

let () = Format.printf "%a" (pp_content_node pp_content) content
