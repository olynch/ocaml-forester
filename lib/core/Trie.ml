(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

include Yuujinchou.Trie

type path = string list
[@@deriving repr]

let pp_path =
  let pp_sep fmt () = Format.pp_print_string fmt "." in
  Format.pp_print_list ~pp_sep Format.pp_print_string
