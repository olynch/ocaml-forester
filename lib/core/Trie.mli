(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

include module type of Yuujinchou.Trie

val path_t : path Repr.t
val pp_path : Format.formatter -> path -> unit
