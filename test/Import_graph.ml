(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Forester_forest
open Forester_prelude
open Forester_frontend

module T = Types

let config = { Config.default_forest_config with trees = ["imports"] }

let () =
  let@ env = Eio_main.run in
  let open Alcotest in
  let forest =
    Compiler.(
      init ~env ~config
      |> load (Eio_util.paths_of_dirs ~env config.trees)
      |> parse ~quit_on_error: false
    )
  in
  let g =
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
  let has_edge v w = Forest_graph.mem_edge g v w in
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
      (Forest.length forest.parsed >= (Forest_graph.nb_vertex g));
    Alcotest.(check bool)
      "has edge"
      true
      (
        List.for_all
          Fun.id
          [
            (has_edge vtx_b vtx_a);
            (has_edge vtx_c vtx_b);
            (has_edge vtx_d vtx_c);
            (has_edge vtx_e vtx_c);
          ]
      )
  in
  run
    "Import graph"
    [
      "creating import graph",
      [
        test_case "check parsed trees" `Quick test_parsed_trees;
        test_case "check number of vertices" `Quick test_graph;
      ]
    ]
