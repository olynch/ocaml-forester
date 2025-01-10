(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_frontend

let () =
  let@ env = Eio_main.run in
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);
  let@ () = Reporter.easy_run in
  let config = Forester_forest.Config.parse_forest_config_file "forest.toml" in
  let (state, result) =
    Machine.(
      init ~env ~config
      |> run_until Do_nothing
    )
  in
  Machine.serve ~env state
(* match result with *)
(* | Nothing -> Format.printf "Got nothing@." *)
(* | Vertex_set _ -> Format.printf "Got vertex set@." *)
(* | Render_result node -> Format.printf "Got render result: %a@." Pure_html.pp node *)
(* | Error (`Not_found i) -> Format.printf "Got error: %a not found@." pp_iri i *)
