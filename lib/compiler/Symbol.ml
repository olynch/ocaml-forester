(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
type t = (Trie.path [@repr Repr.(list string)]) * int
[@@deriving repr]

let counter = ref 0

let named path =
  counter := !counter + 1;
  path, !counter

let fresh () = named []

let clone (path, _) = named path

let pp fmt (sym, ix) =
  Format.fprintf fmt "%a@%i" Trie.pp_path sym ix

let show x = Format.asprintf "%a" pp x

let compare = compare

let repr : t Repr.t =
  Repr.pair (Repr.list Repr.string) Repr.int
