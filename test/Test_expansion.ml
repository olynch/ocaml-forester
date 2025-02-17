(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler
open Forester_frontend
open Testables

let () =
  let open Alcotest in
  let host = "test" in
  let _, env, result =
    (
      Expand.expand_tree
        ~quit_on_error: true
        ~host
        Expand.Env.empty
        {
          source_path = None;
          (* If tree has no address, exports are not added *)
          iri = Some (Iri_scheme.user_iri ~host "test-tree");
          code = let open DSL.Code in
          [
            def
              ["foo"]
              []
              [ident ["p"]; braces []];
            ident ["foo"]
          ]
        }
    )
  in
  let test_result () =
    let open Forester_frontend.DSL.Syn in
    Alcotest.(check syn)
      "works"
      [fun_ [] [prim `P; braces []]]
      result.syn
  in
  let test_exports () =
    Alcotest.(check int)
      "number of exports"
      1
      (Expand.Unit_map.cardinal env);
    env
    |> Expand.Unit_map.to_seq
    |> Seq.iter
      (
        fun (this_iri, export) ->
          (
            export
            |> Yuujinchou.Trie.to_seq
            |> Seq.iter
              (
                fun (path, (_data, _)) ->
                  Alcotest.(check (list string))
                    ""
                    ["foo"]
                    path
              )
          );
          Alcotest.(check iri)
            "addr"
            (Iri.of_string "forest://test/test-tree")
            (this_iri)
      )
  in
  let test_suggestions () =
    let module Sc = Resolver.Scope in
    Alcotest.(check (list @@ pair path int))
      "when prompted with baz, should suggest existing binding bar"
      [(["bar"], 1)]
      (
        Sc.easy_run @@
          fun
              ()
            ->
            (* Create a scope that includes binding "bar"*)
            Sc.include_singleton
              ["bar"] @@
              (
                Term
                  [
                    Asai.Range.locate_opt
                      None
                      (Syn.Sym (Symbol.named ["symbol"]))
                  ],
                None
              );
            Expand.suggestions ["baz"] @@ Sc.get_visible ()
            |> List.map (fun (a, _, b) -> (a, b))
      )
  in
  run
    "Expansion"
    [
      "expanding a single tree",
      [
        test_case "check exports" `Quick test_exports;
        test_case "check expansion result" `Quick test_result;
        test_case "check suggestions" `Quick test_suggestions
      ];
    ]
