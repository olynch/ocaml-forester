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
  let config = Config_parser.parse_forest_config_file "forest.toml" in
  State_machine.(
    serve
      ~env
      (
        batch_run ~env ~config
      )
  )
