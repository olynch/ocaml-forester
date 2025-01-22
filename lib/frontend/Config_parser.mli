(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_compiler

val parse_forest_config_string : string -> Config.t
val parse_forest_config_file : string -> Config.t
