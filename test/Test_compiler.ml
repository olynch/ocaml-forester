(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_forest
open Forester_frontend
let do_ a b = a; b

let () =
  Logs.set_level (Some Debug);
  let@ env = Eio_main.run in
  let config = Config.default_forest_config in
  let tree_dirs = Eio_util.paths_of_dirs ~env config.trees in
  let test_reparsing () =
    let@ () = Reporter.test_run in
    let before_forest =
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
    (* let uri = Hashtbl.find_opt (fun _ -> Lsp.Uri.of_string) before_forest in *)
    let reparsed_forest =
      Compiler.(
        before_forest
        |> reparse
          (
            Lsp.Text_document.make
              ~position_encoding: `UTF16 @@
              Lsp.Types.DidOpenTextDocumentParams.create
                ~textDocument: (
                  Lsp.Types.TextDocumentItem.create
                    ~languageId: "forester"
                    ~uri: (Lsp.Uri.of_path "")
                    ~version: 2
                    ~text: {| \title{I am now importing something and the import graphs should be updated accordingly} \import{a}|}
                )
          )
      )
    in
    Alcotest.(check bool) "" true false
  in
  let test () =
    let forest =
      let@ () = Reporter.easy_run in
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
    Alcotest.(check int)
      ""
      7
      (List.length @@ Compiler.get_all_articles forest)
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
