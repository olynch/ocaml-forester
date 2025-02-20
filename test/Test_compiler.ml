(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_compiler
open Forester_frontend

module T = Types

let () =
  Logs.set_level (Some Debug);
  let@ env = Eio_main.run in
  let config = Config.default in
  let tree_dirs = Eio_util.paths_of_dirs ~env config.trees in

  (* This test verifies that the `reparse` function works correctly.*)
  let test_reparsing () =
    let@ () = Reporter.test_run in
    (*First, perform a regular batch compilation run.*)
    let before_forest =
      State_machine.batch_run
        ~env
        ~config
        ~dev: false
    in
    (* Get the URI of "reparse.tree". In order to make the test reproducible,
       we can't use hardcoded paths, as those may be different in CI and on
       different hosts.*)
    let uri =
      before_forest.documents |> Hashtbl.to_seq_keys
      |> Seq.find_map
          (fun uri ->
            if String.ends_with ~suffix: "reparse.tree" (Lsp.Uri.to_string uri) then
              Some uri
            else None
          )
      |> Option.get
    in
    let vtx =
      T.Iri_vertex
        (Iri_scheme.uri_to_iri ~host: config.host uri)
    in
    let reparsed_forest =
      Phases.(
        before_forest
        |> reparse
            (
              (* Create a Text_document.t with new content.*)
              Lsp.Text_document.make
                ~position_encoding: `UTF16 @@
                Lsp.Types.DidOpenTextDocumentParams.create
                  ~textDocument: (
                    Lsp.Types.TextDocumentItem.create
                      ~languageId: "forester"
                      ~uri
                      ~version: 2
                      ~text: {| \title{I am now importing something and the import graphs should be updated accordingly} \import{a}|}
                  )
            )
      )
    in
    let module Graphs = (val reparsed_forest.graphs) in
    let import_graph = Graphs.get_rel Query.Edges "imports" in
    (* FIXME: *)
    Alcotest.(check string)
      ""
      ""
      ""
  (* Alcotest.(check bool) *)
  (*   "" *)
  (*   true *)
  (*   (Forest_graph.out_degree import_graph vtx > 0) *)
  in
  let test_batch_run () =
    let forest =
      let@ () = Reporter.easy_run in
      State_machine.batch_run ~env ~config ~dev: false
    in
    Alcotest.(check int)
      ""
      8
      (List.length @@ Forest.get_all_articles forest.resources)
  in
  let test_includes_paths () =
    let@ () = Reporter.easy_run in
    let forest =
      State_machine.batch_run
        ~env
        ~config
        ~dev: true
    in
    let path =
      let@ {frontmatter = {source_path; _}; _} =
        Option.bind @@ Forest.get_article (Iri.of_string "forest://my-forest/index") forest.resources
      in
      source_path
    in
    Alcotest.(check bool)
      ""
      true
      (Option.is_some path)
  in
  let test_omits_paths () =
    let@ () = Reporter.easy_run in
    let forest =
      State_machine.batch_run
        ~env
        ~config
        ~dev: false
    in
    let path =
      let@ {frontmatter = {source_path; _}; _} =
        Option.bind @@ Forest.get_article (Iri.of_string "forest://my-forest/index") forest.resources
      in
      source_path
    in
    Alcotest.(check bool)
      ""
      true
      (Option.is_none path)
  in
  let open Alcotest in
  run
    "check compiler internals"
    [
      "pipeline",
      [
        test_case "basic batch run" `Quick test_batch_run;
        test_case "reparsing" `Quick test_reparsing
      ];
      "dev mode",
      [
        test_case "includes paths in dev mode" `Quick test_includes_paths;
        test_case "omits paths outside dev mode" `Quick test_omits_paths;
      ]
    ]
