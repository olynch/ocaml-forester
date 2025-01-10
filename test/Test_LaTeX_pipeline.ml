(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_prelude
open Forester_compiler.Export_for_test
open Forester_frontend.Export_for_test

let () =
  let test_pipeline () =
    let@ env = Eio_main.run in
    let@ () = Reporter.easy_run in
    try
      let source = LaTeX_template.to_string ~preamble: "" ~body: "$a^2 + b^2 = c^2$" in
      let _ = LaTeX_pipeline.latex_to_svg ~env source in
      Alcotest.(check bool) "succeeded" true true
    with
      | _ -> Alcotest.fail "pipeline failed"
  in
  let open Alcotest in
  run
    "LaTeX_pipeline"
    ["", [test_case "pipeline succeeds" `Quick test_pipeline]]
