(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

val parse_file : string -> (Code.t, Reporter.Message.t Asai.Diagnostic.t) result

val parse_string : string -> (Code.t, Reporter.Message.t Asai.Diagnostic.t) result
