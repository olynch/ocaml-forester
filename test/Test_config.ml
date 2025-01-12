(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Testables
open Forester_compiler
open Forester_frontend

let test_parsing () =
  Alcotest.(check config)
    "is the same"
    Config.{
      host = "test";
      trees = ["trees"];
      home = Some "index";
      prefixes = ["foo"; "bar"; "baz"];
      assets = [];
      foreign = [];
      theme = "theme";
    }
    (
      Forester_core.Reporter.easy_run @@
        fun () ->
          Config_parser.parse_forest_config_string
            {|
        [forest]
        host = "test"
        trees = ["trees"]
        home = "index"
        prefixes = ["foo", "bar", "baz"]
        |}
    )

let () =
  let open Alcotest in
  run
    "Config parsing"
    [
      "example config works",
      [test_case "" `Quick test_parsing];
    ]
