(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler
open Testables
open Prelude
open Forester_frontend.DSL.Code

(* Using local open breaks tests, see https://github.com/mirage/alcotest/issues/414*)

let test_prim () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          ident ["p"];
          braces
            [
              ident ["ul"];
              braces
                [
                  ident ["li"];
                  braces
                    [text "foo"]
                ]
            ]
        ]
    )
    (
      parse_string
        {|\p{\ul{\li{foo}}}|}
    )

let test_open () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (Ok [open_ ["foo"]])
    (parse_string {|\open\foo|});
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (Ok [open_ ["foo"; "bar"; "baz"]])
    (parse_string {|\open\foo/bar/baz|})

let test_scope () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          scope
            [
              ident ["p"];
              braces []
            ]
        ]
    )
    (parse_string {|\scope{\p{}}|})

let test_verbatim () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (Ok [verbatim "asdf"])
    (parse_string {|\verb<<|asdf<<|})

let test_math () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          math
            Inline
            [
              (text "a^2");
              (text " ");
              (text "+");
              (text " ");
              (text "b^2");
              (text " ");
              (text "=");
              (text " ");
              (text "c^2")
            ]
        ]
    )
    (parse_string {|#{a^2 + b^2 = c^2}|});
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          math
            Display
            [
              (text "a^2");
              (text " ");
              (text "+");
              (text " ");
              (text "b^2");
              (text " ");
              (text "=");
              (text " ");
              (text "c^2")
            ]
        ]
    )
    (parse_string {|##{a^2 + b^2 = c^2}|})

let test_object () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (
      Ok
        [
          object_
            {
              self = (Some ["self"]);
              methods = [
                (
                  "foo",
                  []
                )
              ]
            }
        ]
    )
    (
      parse_string
        {|
        \object[self]{
          [foo]{}
        }|}
    )

let test_file_parsing () =
  Alcotest.(check @@ result code diagnostic)
    "same nodes"
    (Ok [ident ["foo"]])
    (
      let@ () = Reporter.easy_run in
      Result.map strip_loc @@
        Parse.parse_file "trees/index.tree"
    )

let () =
  let open Alcotest in
  run
    "Parser"
    [
      "nodes", [test_case "open" `Quick test_open; ];
      "scope", [test_case "scope" `Quick test_scope; ];
      "text", [test_case "text" `Quick test_prim];
      "verbatim", [test_case "verbatim" `Quick test_verbatim];
      "math", [test_case "math" `Quick test_math];
      "object", [test_case "object" `Quick test_object];
      "file", [test_case "parse file" `Quick test_file_parsing];
    ]
