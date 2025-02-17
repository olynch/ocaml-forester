(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Testables
open Forester_core
open Forester_prelude
open Forester_compiler

let test_baseN () =
  Alcotest.(check @@ option int)
    ""
    (Some 460198)
    (BaseN.Base36.int_of_string "9V3A");
  Alcotest.(check string)
    ""
    "9V3A"
    (BaseN.Base36.string_of_int 460198)

let () =
  let open Alcotest in
  run
    "Iri_util"
    [
      "BaseN", [test_case "" `Quick test_baseN];
    ]
