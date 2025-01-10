(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Base

module Message = struct
  type t =
    | Tree_not_found of iri
    | Duplicate_tree of iri
    | Parse_error
    | Type_error
    | Type_warning
    | Resolution_error
    | Resolution_warning
    | Duplicate_attribute
    | Unhandled_case
    | Transclusion_loop
    | Internal_error
    | Configuration_error
    | Initialization_warning
    | Routing_error
    | Profiling
    | External_error
    | Resource_not_found
    | Broken_link
    | IO_error
    | Log
  [@@deriving show]

  let default_severity : t -> Asai.Diagnostic.severity = function
    | Duplicate_tree _ -> Error
    | Tree_not_found _ -> Error
    | Parse_error -> Error
    | Type_error -> Error
    | Type_warning -> Warning
    | Resolution_error -> Error
    | Resolution_warning -> Warning
    | Duplicate_attribute -> Error
    | Unhandled_case -> Bug
    | Transclusion_loop -> Error
    | Internal_error -> Bug
    | Configuration_error -> Error
    | Initialization_warning -> Warning
    | Routing_error -> Error
    | Profiling -> Info
    | External_error -> Error
    | Log -> Info
    | Resource_not_found -> Error
    | Broken_link -> Warning
    | IO_error -> Error

  let short_code : t -> string =
    show
end

include Asai.Reporter.Make(Message)

type diagnostic = Message.t Asai.Diagnostic.t

let profile msg body =
  let before = Unix.gettimeofday () in
  let result = body () in
  let after = Unix.gettimeofday () in
  emitf Profiling "[%fs] %s" (after -. before) msg;
  result

module Tty = Asai.Tty.Make(Message)

let easy_run k =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  run ~emit: Tty.display ~fatal k

let silence k =
  let fatal diagnostics =
    Tty.display diagnostics;
    exit 1
  in
  run ~emit: Tty.display ~fatal k

let test_run k =
  let fatal diagnostics =
    Tty.display
      ~use_color: false
      ~use_ansi: false
      diagnostics;
    exit 1
  in
  let emit _diagnostics = () in
  run ~emit ~fatal k

let lsp_run ?init_loc ?init_backtrace publish path k =
  let diagnostics = ref [] in
  let push_diagnostic d =
    diagnostics := d :: !diagnostics
  in
  run
    ~emit: push_diagnostic
    ~fatal: push_diagnostic
    ?init_loc
    ?init_backtrace @@
    fun () ->
      let result = k () in
      publish path !diagnostics;
      result

let ignore = run ~emit: (fun _ -> ()) ~fatal: (fun _ -> fatalf Message.Internal_error "ignoring error")
