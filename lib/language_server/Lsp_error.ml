(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

(* https://github.com/ocaml/ocaml-lsp/blob/556da72593c88b027a7124ae86da6cd638c7d679/ocaml-lsp-server/src/code_actions/action_jump.ml#L23 *)

type lsp_error =
  | Decode_error of string
  | Handshake_error of string
  | Shutdown_error of string
  | Unknown_request of string
  | Unknown_notification of string

exception Lsp_error of lsp_error

let decode_error s = Lsp_error (Decode_error s)
