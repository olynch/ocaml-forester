(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

type t = [`P | `Ol | `Ul | `Li | `Em | `Strong | `Code | `Blockquote | `Pre | `Figure | `Figcaption]
[@@deriving show, repr]
