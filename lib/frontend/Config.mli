(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module Forest_config: sig
  type t = {
    host: string;
    home: string option;
    trees: string list;
    assets: string list;
    foreign: string list;
    theme: string;
  }
  [@@deriving show]
end

val default_forest_config : Forest_config.t
val parse_forest_config_file : string -> Forest_config.t
