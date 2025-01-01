(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_forest
open Forester_frontend

module EP = Eio.Path

module T = Types

let path_of_dir ~env dir =
  EP.(Eio.Stdenv.fs env / dir)

let paths_of_dirs ~env =
  List.map (path_of_dir ~env)

let config = Config.default_forest_config
let host = config.host

let () =
  let@ env = Eio_main.run in
  let open Alcotest in
  let content = testable T.(pp_content) ( = ) in
  let frontmatter = testable T.(pp_frontmatter pp_content) ( = ) in
  let tree_dirs = paths_of_dirs ~env config.trees in
  let@ () = Reporter.easy_run in
  let forest =
    Compiler.(
      init ~env ~config
      |> load tree_dirs
      |> parse ~quit_on_error: true
      |> build_import_graph
      |> expand ~quit_on_error: true
      |> eval ~dev: false
      |> plant
    )
  in
  let get_article addr =
    Forest.find_opt forest.resources (Iri_scheme.user_iri ~host addr)
    |> function
    | Some (T.Article article) -> Some article
    | _ -> None
  in
  let render_article addr =
    get_article addr
    |> Option.map
      (
        fun article ->
          Format.asprintf "%a" Render.(pp_xml ~forest ~dev: false ?stylesheet: None) article
      )
  in
  let check_documents () =
    Alcotest.(check int)
      "check number of loaded trees"
      6
      (Hashtbl.length forest.documents)
  in
  let check_parsed () =
    Alcotest.(check int)
      "check number of parsed trees"
      6
      (Forest.length forest.parsed)
  in
  let check_expanded () =
    Alcotest.(check int)
      "check number of expanded"
      6
      (Forest.length forest.expanded)
  in
  let check_resources () =
    Alcotest.(check int)
      "check number of resources"
      6
      (Forest.length forest.resources)
  in
  let check_output () =
    Alcotest.(check (option frontmatter))
      "check frontmatter"
      (
        Some
          {
            Types.iri = (Some (Iri_scheme.user_iri ~host "index"));
            title = None;
            dates = [];
            attributions = [];
            taxon = None;
            number = None;
            designated_parent = None;
            source_path = None;
            tags = [];
            metas = []
          }
      )
      (
        get_article "index"
        |> Option.map
          (function article -> T.(article.frontmatter))
      );
    Alcotest.(check (option content))
      "check mainmatter"
      (Some (T.Content [(Types.TeX_cs (TeX_cs.Word "foo"))]))
      (
        get_article "index"
        |> Option.map (function article -> T.(article.mainmatter))
      );
    Alcotest.(check (option content))
      "check backmatter"
      (
        Some
          (
            T.Content
              [
                (
                  Types.Section
                    {
                      Types.frontmatter =
                      {
                        Types.iri = None;
                        title = (Some (Types.Content [(Types.Text "references")]));
                        dates = [];
                        attributions = [];
                        taxon = None;
                        number = None;
                        designated_parent = None;
                        source_path = None;
                        tags = [];
                        metas = []
                      };
                      mainmatter = (
                        Types.Content
                          [
                            (
                              Types.Results_of_datalog_query
                                {
                                  Datalog_expr.var = "X";
                                  positives = [
                                    {
                                      Datalog_expr.rel = "org.forester.rel.references";
                                      args = [
                                        (
                                          Datalog_expr.Const
                                            (
                                              Types.Iri_vertex
                                                (Iri_scheme.user_iri ~host "index")
                                            )
                                        );
                                        (Datalog_expr.Var "X")
                                      ]
                                    }
                                  ];
                                  negatives = []
                                }
                            )
                          ]
                      );
                      flags =
                      {
                        Types.hidden_when_empty = (Some true);
                        included_in_toc = None;
                        header_shown = None;
                        metadata_shown = (Some false);
                        numbered = None;
                        expanded = None
                      }
                    }
                );
                (
                  Types.Section
                    {
                      Types.frontmatter =
                      {
                        Types.iri = None;
                        title = (Some (Types.Content [(Types.Text "context")]));
                        dates = [];
                        attributions = [];
                        taxon = None;
                        number = None;
                        designated_parent = None;
                        source_path = None;
                        tags = [];
                        metas = []
                      };
                      mainmatter = (
                        Types.Content
                          [
                            (
                              Types.Results_of_datalog_query
                                {
                                  Datalog_expr.var = "X";
                                  positives = [
                                    {
                                      Datalog_expr.rel = "org.forester.rel.transcludes";
                                      args = [
                                        (Datalog_expr.Var "X");
                                        (
                                          Datalog_expr.Const
                                            (
                                              Types.Iri_vertex
                                                (Iri_scheme.user_iri ~host "index")
                                            )
                                        )
                                      ]
                                    }
                                  ];
                                  negatives = []
                                }
                            )
                          ]
                      );
                      flags =
                      {
                        Types.hidden_when_empty = (Some true);
                        included_in_toc = None;
                        header_shown = None;
                        metadata_shown = (Some false);
                        numbered = None;
                        expanded = None
                      }
                    }
                );
                (
                  Types.Section
                    {
                      Types.frontmatter =
                      {
                        Types.iri = None;
                        title = (Some (Types.Content [(Types.Text "backlinks")]));
                        dates = [];
                        attributions = [];
                        taxon = None;
                        number = None;
                        designated_parent = None;
                        source_path = None;
                        tags = [];
                        metas = []
                      };
                      mainmatter = (
                        Types.Content
                          [
                            (
                              Types.Results_of_datalog_query
                                {
                                  Datalog_expr.var = "X";
                                  positives = [
                                    {
                                      Datalog_expr.rel = "org.forester.rel.links-to";
                                      args = [
                                        (Datalog_expr.Var "X");
                                        (
                                          Datalog_expr.Const
                                            (
                                              Types.Iri_vertex
                                                (Iri_scheme.user_iri ~host "index")
                                            )
                                        )
                                      ]
                                    }
                                  ];
                                  negatives = []
                                }
                            )
                          ]
                      );
                      flags =
                      {
                        Types.hidden_when_empty = (Some true);
                        included_in_toc = None;
                        header_shown = None;
                        metadata_shown = (Some false);
                        numbered = None;
                        expanded = None
                      }
                    }
                );
                (
                  Types.Section
                    {
                      Types.frontmatter =
                      {
                        Types.iri = None;
                        title = (Some (Types.Content [(Types.Text "related")]));
                        dates = [];
                        attributions = [];
                        taxon = None;
                        number = None;
                        designated_parent = None;
                        source_path = None;
                        tags = [];
                        metas = []
                      };
                      mainmatter = (
                        Types.Content
                          [
                            (
                              Types.Results_of_datalog_query
                                {
                                  Datalog_expr.var = "X";
                                  positives = [
                                    {
                                      Datalog_expr.rel = "org.forester.rel.links-to";
                                      args = [
                                        (
                                          Datalog_expr.Const
                                            (
                                              Types.Iri_vertex
                                                (Iri_scheme.user_iri ~host "index")
                                            )
                                        );
                                        (Datalog_expr.Var "X")
                                      ]
                                    }
                                  ];
                                  negatives = [
                                    {
                                      Datalog_expr.rel = "org.forester.rel.is-reference";
                                      args = [(Datalog_expr.Var "X")]
                                    }
                                  ]
                                }
                            )
                          ]
                      );
                      flags =
                      {
                        Types.hidden_when_empty = (Some true);
                        included_in_toc = None;
                        header_shown = None;
                        metadata_shown = (Some false);
                        numbered = None;
                        expanded = None
                      }
                    }
                );
                (
                  Types.Section
                    {
                      Types.frontmatter =
                      {
                        Types.iri = None;
                        title = (
                          Some
                            (
                              Types.Content
                                [(Types.Text "contributions")]
                            )
                        );
                        dates = [];
                        attributions = [];
                        taxon = None;
                        number = None;
                        designated_parent = None;
                        source_path = None;
                        tags = [];
                        metas = []
                      };
                      mainmatter = (
                        Types.Content
                          [
                            (
                              Types.Results_of_datalog_query
                                {
                                  Datalog_expr.var = "X";
                                  positives = [
                                    {
                                      Datalog_expr.rel = "org.forester.rel.has-direct-contributor";
                                      args = [
                                        (Datalog_expr.Var "X");
                                        (
                                          Datalog_expr.Const
                                            (
                                              Types.Iri_vertex
                                                (Iri_scheme.user_iri ~host "index")
                                            )
                                        )
                                      ]
                                    }
                                  ];
                                  negatives = []
                                }
                            )
                          ]
                      );
                      flags =
                      {
                        Types.hidden_when_empty = (Some true);
                        included_in_toc = None;
                        header_shown = None;
                        metadata_shown = (Some false);
                        numbered = None;
                        expanded = None
                      }
                    }
                )
              ]
          )
      )
      (
        get_article "index"
        |> Option.map (function article -> T.(article.backmatter))
      );
  in
  let _check_rendered () =
    Alcotest.(check (option string))
      "check transcluder"
      (
        Some
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n<fr:tree\nxmlns:fr=\"http://www.jonmsterling.com/jms-005P.xml\"\nroot=\"false\"><fr:frontmatter><fr:authors /><fr:anchor>385</fr:anchor><fr:addr\ntype=\"user\">transcluder</fr:addr><fr:route>transcluder.xml</fr:route><fr:title\ntext=\"\" /></fr:frontmatter><fr:mainmatter><fr:p>bar</fr:p><fr:tree\nshow-metadata=\"false\"><fr:frontmatter><fr:authors /><fr:anchor>384</fr:anchor><fr:addr\ntype=\"user\">transcludee</fr:addr><fr:route>transcludee.xml</fr:route><fr:title\ntext=\"\" /></fr:frontmatter><fr:mainmatter><fr:p>I am being transcluded</fr:p></fr:mainmatter></fr:tree><fr:p><fr:link\nhref=\"linked.xml\"\ntitle=\"Link to me\"\naddr=\"linked\"\ntype=\"local\">link</fr:link></fr:p></fr:mainmatter><fr:backmatter><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>377</fr:anchor><fr:title\ntext=\"References\">References</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>378</fr:anchor><fr:title\ntext=\"Context\">Context</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>380</fr:anchor><fr:title\ntext=\"Backlinks\">Backlinks</fr:title></fr:frontmatter><fr:mainmatter><fr:tree\nshow-metadata=\"true\"\nexpanded=\"false\"\ntoc=\"false\"\nnumbered=\"false\"><fr:frontmatter><fr:authors /><fr:anchor>379</fr:anchor><fr:addr\ntype=\"user\">linker</fr:addr><fr:route>linker.xml</fr:route><fr:title\ntext=\"\" /></fr:frontmatter><fr:mainmatter><fr:p><fr:link\nhref=\"transcluder.xml\"\ntitle=\"\"\naddr=\"transcluder\"\ntype=\"local\">link</fr:link></fr:p></fr:mainmatter></fr:tree></fr:mainmatter></fr:tree><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>382</fr:anchor><fr:title\ntext=\"Related\">Related</fr:title></fr:frontmatter><fr:mainmatter><fr:tree\nshow-metadata=\"true\"\nexpanded=\"false\"\ntoc=\"false\"\nnumbered=\"false\"><fr:frontmatter><fr:authors /><fr:anchor>381</fr:anchor><fr:addr\ntype=\"user\">linked</fr:addr><fr:route>linked.xml</fr:route><fr:title\ntext=\"Link to me\">Link to me</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree></fr:mainmatter></fr:tree><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>383</fr:anchor><fr:title\ntext=\"Contributions\">Contributions</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree></fr:backmatter></fr:tree>"
      )
      (render_article "transcluder");
    Alcotest.(check (option string))
      "check XML"
      (
        Some
          "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n\n<fr:tree\nxmlns:fr=\"http://www.jonmsterling.com/jms-005P.xml\"\nroot=\"false\"><fr:frontmatter><fr:authors /><fr:anchor>391</fr:anchor><fr:addr\ntype=\"user\">index</fr:addr><fr:route>index.xml</fr:route><fr:title\ntext=\"\" /></fr:frontmatter><fr:mainmatter>\\foo</fr:mainmatter><fr:backmatter><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>386</fr:anchor><fr:title\ntext=\"References\">References</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>387</fr:anchor><fr:title\ntext=\"Context\">Context</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>388</fr:anchor><fr:title\ntext=\"Backlinks\">Backlinks</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>389</fr:anchor><fr:title\ntext=\"Related\">Related</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree><fr:tree\nshow-metadata=\"false\"\nhidden-when-empty=\"true\"><fr:frontmatter><fr:anchor>390</fr:anchor><fr:title\ntext=\"Contributions\">Contributions</fr:title></fr:frontmatter><fr:mainmatter /></fr:tree></fr:backmatter></fr:tree>"
      )
      (render_article "index")
  in
  run
    "Output"
    [
      "works",
      [
        test_case "has correct number of documents" `Quick check_documents;
        test_case "has correct number of parsed trees" `Quick check_parsed;
        test_case "has correct number of expanded trees" `Quick check_expanded;
        test_case "has correct number of resources" `Quick check_resources;
        test_case "check output" `Quick check_output;
        (* test_case "check rendered XML" `Quick check_rendered; *)
        (* test_case "output is correct" `Quick test *)
      ]
    ]
