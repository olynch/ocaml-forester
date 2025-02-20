(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_frontend.DSL
open Forester_core
open Query
module T = Types

let content_node =
(module struct
  type t = T.content T.content_node
  let equal = (=)
  let pp = T.pp_content_node T.pp_content
end: Alcotest.TESTABLE with type t = T.content T.content_node)

let content =
  p
    [
      ol
        [
          li [txt "First item"];
          li [txt "Second item"]
        ];
      ul
        [
          li [txt "First item"];
          li [txt "Second item"]
        ];
      section ~mainmatter: ([p [txt "section"]]) ();
      em [txt "Emphasized item"];
      strong [txt "Strong text"];
      code [txt "fun _ -> ()"];
      blockquote [txt "blockquote"];
      pre [txt "pre"];
      figure [txt "figure"];
      figcaption [txt "caption"];
      cdata "cdata";
      xml_elt (None, "html") [];
      transclude "foo-001";
      contextual_number "chapter-3";
      results_of_query (union []);
      katex Inline (T.Content [txt "a = b"]);
      tex "\\begin{}";
      link "https://git.sr.ht/~jonsterling/ocaml-forester" [txt "Forester"];
      img "img.png";
      artefact [txt "res"];
    ]

let test () =
  Alcotest.(check @@ content_node)
    "works"
    (
      Prim
        (
          `P,
          (
            Content
              [
                (
                  Prim
                    (
                      `Ol,
                      (Content [(Prim (`Li, (Content [(Text "First item")]))); (Prim (`Li, (Content [(Text "Second item")])))])
                    )
                );
                (
                  Prim
                    (
                      `Ul,
                      (
                        Content
                          [
                            (
                              Prim
                                (
                                  `Li,
                                  (
                                    Content
                                      [(Text "First item")]
                                  )
                                )
                            );
                            (Prim (`Li, (Content [(Text "Second item")])))
                          ]
                      )
                    )
                );
                (
                  Section
                    {
                      frontmatter = {iri = None; title = None; dates = []; attributions = []; taxon = None; number = None; designated_parent = None; source_path = None; tags = []; metas = []};
                      mainmatter = (Content [(Prim (`P, (Content [(Text "section")])))]);
                      flags = {hidden_when_empty = None; included_in_toc = None; header_shown = None; metadata_shown = (Some false); numbered = None; expanded = None}
                    }
                );
                (Prim (`Em, (Content [(Text "Emphasized item")])));
                (Prim (`Strong, (Content [(Text "Strong text")])));
                (Prim (`Code, (Content [(Text "fun _ -> ()")])));
                (Prim (`Blockquote, (Content [(Text "blockquote")])));
                (Prim (`Pre, (Content [(Text "pre")])));
                (Prim (`Figure, (Content [(Text "figure")])));
                (Prim (`Figcaption, (Content [(Text "caption")])));
                (CDATA "cdata");
                (Xml_elt {name = {prefix = ""; uname = "html"; xmlns = None}; attrs = []; content = (Content [])});
                (Transclude {href = Iri.of_string "foo-001"; target = Mainmatter; modifier = Identity});
                (Contextual_number (Iri.of_string "chapter-3"));
                (Results_of_query (Query.Union []));
                (KaTeX (Inline, (Content [(Text "a = b")])));
                (TeX_cs (Word {|\begin{}|}));
                (Link {href = Iri.of_string "https://git.sr.ht/~jonsterling/ocaml-forester"; content = (Content [(Text "Forester")])});
                (Img (Remote "img.png"));
                (Artefact {hash = ""; content = (Content [(Text "res")]); sources = []})
              ]
          )
        )
    )
    content

let () =
  let open Alcotest in
  run "DSL" ["works", [test_case "" `Quick test]]
