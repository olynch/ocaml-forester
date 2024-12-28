(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_frontend
open Forester_forest

let () =
  Logs.set_level (Some Debug);
  let@ env = Eio_main.run in
  let config = Config.default_forest_config in
  let tree_dirs = Eio_util.paths_of_dirs ~env config.trees in
  let init = Compiler.init ~env ~config in
  let loaded = Compiler.load tree_dirs init in
  Eio.traceln "%i" (Hashtbl.length @@ Compiler.documents loaded);
  let parsed = Compiler.parse ~quit_on_error: true loaded in
  Eio.traceln "%i" (Forest.length @@ Compiler.parsed parsed)
