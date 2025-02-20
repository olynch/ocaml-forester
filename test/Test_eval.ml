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

let eval_string ~iri ~host str =
  str
  |> Prelude.parse_string
  |> Result.get_ok
  |> (fun code -> Code.{code; source_path = None; iri = None;})
  |> Expand.expand_tree ~quit_on_error: false ~host Expand.Env.empty
  |> (fun (_, _, syn) ->
      Eval.eval_tree ~host ~iri ~source_path: None syn
    )
  |> (fun (ds, {articles; _}) -> (ds, (List.hd articles).mainmatter))

let () =
  Logs.set_level (Some Debug);
  let config = Config.default in
  let host = config.host in
  let iri = Iri_scheme.user_iri ~host "test" in
  let open Forester_frontend.DSL in
  let open Forester_frontend.DSL.Datalog in
  let test_eval str res =
    Alcotest.(check (pair (list Testables.diagnostic) Testables.content))
      ""
      (eval_string ~iri ~host str)
      res
  in
  let test_verbatim () = test_eval {|\verb<<|asdf<<|} ([], T.Content [cdata "asdf"]) in
  let test_datalog () =
    test_eval
      {|\execute\datalog{
 \rel/accepted-or-refereed ?X -: {\rel/has-tag ?X '{refereed}}
      }|}
      ([], T.Content [])
  in
  let test_let () =
    test_eval
      {|\let\foo[x]{\x}
      \foo{bar}
      |}
      ([], T.Content [txt "bar"])
  in
  let test_def () =
    test_eval
      {|\def\foo[x]{\x}
      \foo{bar}
      |}
      ([], T.Content [txt "bar"])
  in
  let test_object () =
    test_eval
      {|
\def\my-object{
  \object[self]{
    [method1]{the implementation of this method}
    [method2]{
      the implementation of another method
      \self#method1
    }
  }
}
\my-object#method1
      |}
      (
        [],
        T.Content
          [
            txt "the";
            txt " ";
            txt "implementation";
            txt " ";
            txt "of";
            txt " ";
            txt "this";
            txt " ";
            txt "method";
          ]
      )
  in
  let open Alcotest in
  run
    "check evaluator"
    [
      "verbatim", [test_case "" `Quick test_verbatim];
      "objects",
      [
        test_case "" `Quick test_object;
      ];
      "datalog",
      [
        test_case "" `Quick test_datalog
      ];
      "function",
      [
        test_case "" `Quick test_let;
        test_case "" `Quick test_def;
      ];
    ]
