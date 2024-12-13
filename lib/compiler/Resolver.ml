(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module P = struct
  type data =
    | Term of Syn.t
    | Xmlns of { xmlns: string; prefix: string }

  type tag = Asai.Range.t option

  type hook = unit (* for modifier hooks; unused here *)
  type context = unit (* for advanced printing and reporting; unused here *)
end

module Scope = struct
  include Yuujinchou.Scope.Make(P)
  type data = P.data

  let import_singleton x v =
    import_singleton (x, v)

  let include_singleton x v =
    include_singleton (x, v)

  let import_subtree ?modifier path subtree =
    import_subtree ?modifier (path, subtree)

  let include_subtree ?modifier path subtree =
    include_subtree ?modifier (path, subtree)

  let easy_run kont = run kont

  let pp_path ppf path =
    let pp_slash ppf () = Format.fprintf ppf "/" in
    Format.(fprintf ppf "%a" (pp_print_list ~pp_sep: pp_slash pp_print_string) path)
end

module Lang = Yuujinchou.Language
