(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Testables
open Forester_core
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
      foreign = ["foreign/forest.json"];
      theme = "theme";
    }
    begin
      Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        host = "test"
        trees = ["trees"]
        home = "index"
        prefixes = ["foo", "bar", "baz"]
        foreign = ["foreign/forest.json"]
        |}
    end

let test_missing_fields () =
  Alcotest.(check config)
    "is the same"
    Config.{
      host = "test";
      trees = ["trees"];
      home = Some "index";
      theme = "theme";
      assets = [];
      foreign = [];
      prefixes = [];
    }
    (
      Forester_core.Reporter.easy_run @@ fun () ->
      Config_parser.parse_forest_config_string
        {|
        [forest]
        host = "test"
        trees = ["trees"]
        home = "index"
        |}
    )

let test_missing_host () =
  Alcotest.(check unit)
    "is the same"
    ()
    begin
      let fatal = function
        | {message; explanation; _} ->
          Alcotest.(check Testables.message) "" Configuration_error message;
          Alcotest.(check string)
            ""
            (Asai.Diagnostic.string_of_text explanation.value)
            "You need to set the `host' key in your configuration file; this is a global identifier that will be used to distinguish your forest from other forests (you can use your name, e.g. `johnqpublic')";
      in
      let emit = function
        | {message; explanation; _} ->
          Alcotest.(check Testables.message) "" Configuration_error message
      in
      Forester_core.Reporter.run ~fatal ~emit @@ fun () ->
      let _ =
        Config_parser.parse_forest_config_string
          {|
        [forest]
        trees = ["trees"]
        home = "index"
        |}
      in
      assert false
    end

let test_parse_error () =
  Alcotest.(check unit)
    "is the same"
    ()
    begin
      Forester_core.Reporter.run
        ~fatal: (function
          | {explanation; _} ->
            Alcotest.(check string)
              ""
              (Asai.Diagnostic.string_of_text explanation.value)
              "Error in <anonymous> at line 2 at column 11 (position 12)";
        )
        ~emit: (fun _ -> ())
        @@ fun () ->
        let _ =
          Config_parser.parse_forest_config_string
            {|
          ]]]
        |}
        in
        assert false
    end

let test_stylesheet_warning () =
  Alcotest.(check config)
    "is the same"
    Config.{
      host = "test";
      trees = ["trees"];
      home = Some "index";
      theme = "theme";
      assets = [];
      foreign = [];
      prefixes = [];
    }
    begin
      let fatal _ = assert false in
      let emit = function
        | {explanation; _} ->
          (
            Alcotest.(check string)
              ""
              "Custom XSL stylesheet injection is no longer supported; please remove the `stylesheet' key from the [forest] group."
              (Asai.Diagnostic.string_of_text explanation.value)
          )
      in
      Forester_core.Reporter.run ~fatal ~emit @@ fun () ->
      Config_parser.parse_forest_config_string
        {|[forest]
        host = "test"
        trees = ["trees"]
        home = "index"
        stylesheet = "custom.xsl"
        |}
    end

let test_root_warning () =
  Alcotest.(check config)
    "is the same"
    Config.{
      host = "test";
      trees = ["trees"];
      home = None;
      theme = "theme";
      assets = [];
      foreign = [];
      prefixes = [];
    }
    begin
      let fatal _ = assert false in
      let emit = function
        | {explanation; _} ->
          Alcotest.(check string)
            ""
            "In your configuration file, change `root' key to `home' in the [forest] group."
            (Asai.Diagnostic.string_of_text explanation.value)
      in
      Forester_core.Reporter.run ~fatal ~emit @@ fun () ->
      Config_parser.parse_forest_config_string
        {|[forest]
        host = "test"
        trees = ["trees"]
        root = "index"
        |}
    end

let () =
  let open Alcotest in
  run
    "Config parsing"
    [
      "example config works",
      [
        test_case "it parses correctly" `Quick test_parsing;
      ];
      "can parse config with missing fields",
      [
        test_case "" `Quick test_missing_fields;
      ];
      "handles errors correctly",
      [
        test_case "" `Quick test_missing_host;
        test_case "" `Quick test_parse_error;
        test_case "" `Quick test_stylesheet_warning;
        test_case "" `Quick test_root_warning;
      ];
    ]
