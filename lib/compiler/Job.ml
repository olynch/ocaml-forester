(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core
open Types

type publication_format =
(* | Rss *)
| Json_blob

type latex_to_svg_job = {
  hash: string;
  source: string;
  content: svg: string -> content
}

type publication = {
  name: string;
  format: publication_format;
  query: dx_query
}

type job =
  | LaTeX_to_svg of latex_to_svg_job
  | Publish of publication
