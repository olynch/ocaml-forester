(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 *)

open Forester_prelude
open Forester_compiler
open Forester_frontend
open Testables

open Export_for_test

let () =
  let@ env = Eio_main.run in
  let open Alcotest in
  Logs.set_reporter (Logs_fmt.reporter ());
  let forest =
    Phases.init
      ~env
      ~config: Config.default
      ~dev: false
  in
  let test_dir_scanner () =
    let dirs = Eio_util.paths_of_dirs ~env Config.default.trees in
    Alcotest.(check @@ list @@ list string)
      ""
      [
        ["trees"; "importee.tree"];
        ["trees"; "importer.tree"];
        ["trees"; "index.tree"];
        ["trees"; "linked.tree"];
        ["trees"; "linker.tree"];
        ["trees"; "reparse.tree"];
        ["trees"; "transcludee.tree"];
        ["trees"; "transcluder.tree"]
      ]
      (
        Dir_scanner.scan_directories dirs
        |> List.of_seq
      )
  in
  let test_find () =
    let dirs = Eio_util.paths_of_dirs ~env Config.default.trees in
    Alcotest.(check @@ option string)
      ""
      (Some "trees/index.tree")
      (
        Iri_resolver.find
          (Iri.of_string "forest://my-forest/index.tree")
          ~in_paths: dirs
      )
  in
  let test_resolve_unloaded target () =
    Alcotest.(check bool)
      ""
      true
      (
        Iri_resolver.resolve
          (* NOTE: no .tree at the end! *)
          (Iri (Iri.of_string "forest://my-forest/index"))
          target
          forest
        |> Option.is_some
      )
  in
  run
    "Iri_resolver"
    [
      "Dir_scanner",
      [
        test_case "" `Quick test_dir_scanner;
      ];
      "find",
      [
        test_case "" `Quick test_find;
      ];
      "resolving_unloaded",
      [
        test_case "resolve to code" `Quick (test_resolve_unloaded To_code);
        test_case "resolve to uri" `Quick (test_resolve_unloaded To_uri);
        test_case "resolve to path" `Quick (test_resolve_unloaded To_path);
      ];
      "resolving loaded",
      [
        (* test_case "" `Quick test_resolve_loaded_to_uri; *)
        (* test_case "" `Quick test_resolve_loaded_to_path; *)
        (* test_case "" `Quick test_resolve_loaded_to_doc; *)
        (* test_case "" `Quick test_resolve_loaded_to_code; *)
      ]
    ]
