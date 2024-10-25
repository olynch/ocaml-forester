(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Base

val scheme : string

val base_iri :
  host: string ->
  iri

val user_iri :
  host: string ->
  string ->
  iri

val hash_iri :
  host: string ->
  string ->
  iri

val fresh :
  host: string ->
  iri

val is_named_iri : iri -> bool

val relativise_iri :
  host: string ->
  iri ->
  iri
