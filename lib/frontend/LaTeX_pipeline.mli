(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type env = Eio_unix.Stdenv.base

val latex_to_svg : env: env -> string -> string
