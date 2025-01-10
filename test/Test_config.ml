(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

(* This file is impacted by https://github.com/mirage/alcotest/issues/414*)
open Testables

let test_parsing () =
  Alcotest.(check config)
    "is the same"
    Forester_forest.Config.Forest_config.{
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
          Forester_forest.Config.parse_forest_config_string
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
