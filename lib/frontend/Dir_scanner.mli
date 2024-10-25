(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

val scan_directories : Eio.Fs.dir_ty Eio.Path.t list -> string list Seq.t
