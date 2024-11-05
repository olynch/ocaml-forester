(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

let path_to_iri ~host str =
  str
  |> String.split_on_char '/'
  |> List.rev
  |> List.hd
  |> Filename.chop_extension
  |> Iri_scheme.user_iri ~host

let uri_to_addr path =
  path
  |> String.split_on_char '/'
  |> List.rev
  |> List.hd
  |> Filename.chop_extension
