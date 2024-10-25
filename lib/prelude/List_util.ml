(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Bwd

let nub xs =
  let rec loop acc = function
    | [] -> Bwd.prepend acc []
    | x :: xs ->
      let acc = if Bwd.mem x acc then acc else Bwd.snoc acc x in
      loop acc xs
  in
  loop Bwd.Emp xs
