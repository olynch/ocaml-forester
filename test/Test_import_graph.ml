(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_compiler
open Forester_prelude
open Forester_frontend

module T = Types

let config = { Config.default with trees = ["imports"] }

let () =
  let@ env = Eio_main.run in
  (* Logs.set_level (Some Debug); *)
  Logs.set_reporter (Logs_fmt.reporter ());
  let open Alcotest in
  let forest =
    State_machine.(
      Phases.init ~env ~config ~dev: false
      |> run_action Load_all ~until: Expand_all
    )
  in
  let batch_graph =
    Imports.run_builder
      {
        forest;
        graph = Forest_graph.create ();
        follow = false
      }
  in
  let mk_vtx addr = T.Iri_vertex (Iri_scheme.user_iri ~host: config.host addr) in
  let vtx_a = mk_vtx "a" in
  let vtx_b = mk_vtx "b" in
  let vtx_c = mk_vtx "c" in
  let vtx_d = mk_vtx "d" in
  let vtx_e = mk_vtx "e" in
  let has_edge g v w = Forest_graph.mem_edge g v w in
  let test_parsed_trees () =
    Alcotest.(check bool)
      "number of trees"
      true
      ((Hashtbl.length forest.documents) = (Forest.length forest.parsed))
  in
  let test_graph () =
    Alcotest.(check bool)
      "number of vertices"
      true
      (Forest.length forest.parsed >= (Forest_graph.nb_vertex batch_graph));
    Alcotest.(check bool)
      "has edges"
      true
      (
        List.for_all
          Fun.id
          [
            (has_edge batch_graph vtx_b vtx_a);
            (has_edge batch_graph vtx_c vtx_b);
            (has_edge batch_graph vtx_d vtx_c);
            (has_edge batch_graph vtx_e vtx_c);
          ]
      )
  in
  let test_minimal_graph () =
    let minimal_graph =
      Imports.run_builder
        ~root: (Iri_scheme.user_iri ~host: config.host "a")
        {
          forest = Phases.init ~env ~dev: false ~config;
          graph = Forest_graph.create ();
          follow = true
        }
    in
    (* Although the imports directory contains more trees, the graph only has 5
       vertices.*)
    Alcotest.(check int)
      "minmal graph has correct number vertices"
      5
      (Forest_graph.nb_vertex minimal_graph)
  in
  let test_phase () =
    let iri = Iri_scheme.user_iri ~host: config.host "a" in
    Alcotest.(check bool)
      ""
      true
      ( try let _ = (State_machine.render_tree ~env ~config ~dev: false HTML iri) in true with _ -> false)
  in
  run
    "Import graph"
    [
      "creating import graph",
      [
        test_case "check parsed trees" `Quick test_parsed_trees;
        test_case "check number of vertices" `Quick test_graph;
      ];
      "creating minimal import graph",
      [
        test_case "running builder" `Quick test_minimal_graph;
        test_case "run compilation phase" `Quick test_phase;
        (* test_case "check dependencies" `Quick test_dependencies; *)
      ]
    ]
