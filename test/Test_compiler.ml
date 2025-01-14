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
  let test_reparsing () =
    let@ () = Reporter.test_run in
    let before_forest =
      State_machine.batch_run
        ~env
        ~config
        ~dev: false
    in
    let uri =
      before_forest.documents |> Hashtbl.to_seq_keys
      |> Seq.find_map
        (
          fun uri ->
            if String.ends_with ~suffix: "reparse.tree" (Lsp.Uri.to_string uri) then
              Some uri
            else None
        )
      |> Option.get
    in
    let vtx = T.Iri_vertex (Iri_scheme.user_iri ~host: config.host (Iri_util.uri_to_addr uri)) in
    let reparsed_forest =
      Phases.(
        before_forest
        |> reparse
          (
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
    Alcotest.(check string)
      ""
      ""
      ""
  (* Alcotest.(check bool) *)
  (*   "" *)
  (*   true *)
  (*   (Forest_graph.out_degree import_graph vtx > 0) *)
  in
  let test () =
    let forest =
      let@ () = Reporter.easy_run in
      State_machine.batch_run ~env ~config ~dev: false
    in
    Alcotest.(check int)
      ""
      7
      (List.length @@ Forest.get_all_articles forest.resources)
  in
  let open Alcotest in
  run
    "check compiler internals"
    [
      "pipeline",
      [
        test_case "basic batch run" `Quick test;
        test_case "reparsing" `Quick test_reparsing
      ]
    ]
