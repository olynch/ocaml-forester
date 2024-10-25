(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

let override x y =
  match x with
  | Some _ -> x
  | None -> y

let guard p x =
  if p x then Some x else None
