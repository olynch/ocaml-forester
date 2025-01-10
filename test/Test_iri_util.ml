(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Testables
open Forester_forest
open Forester_core

let test_iri_to_addr () =
  Alcotest.(check string)
    ""
    (Iri_util.iri_to_addr (Iri.of_string "/foo/bar"))
    "bar"

let () =
  let open Alcotest in
  run
    "Iri_util"
    [
      "iri_to_addr", [test_case "" `Quick test_iri_to_addr]
    ]
