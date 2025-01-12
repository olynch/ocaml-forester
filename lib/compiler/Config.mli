(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type t = {
  host: string;
  home: string option;
  trees: string list;
  assets: string list;
  foreign: string list;
  theme: string;
  prefixes: string list;
}
[@@deriving show]

val default : t
