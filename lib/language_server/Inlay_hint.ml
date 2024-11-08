(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core

module L = Lsp.Types
module F = Analysis.F
module PT = Analysis.PT

let (let*) = Option.bind

let compute (params : L.InlayHintParams.t) : L.InlayHint.t list option =
  match params with
  | {
    textDocument;
    _;
  } ->
    let server = State.get () in
    match Hashtbl.find_opt server.index.codes { uri = textDocument.uri } with
    | None -> None
    | Some { code; _ } ->
      List.filter_map
        (
          fun
              (Range.{ loc; _ } as node)
            ->
            match Option.map Range.view loc with
            | Some (`Range (_, pos)) ->
              let* str = Analysis.extract_addr node in
              let iri = Iri_scheme.user_iri ~host: server.config.host str in
              let* { frontmatter; _ } = F.get_article iri in
              let* title = frontmatter.title in
              let content = " " ^ PT.string_of_content title in
              Some
                (
                  L.InlayHint.create
                    ~position: (Lsp_shims.Loc.lsp_pos_of_pos pos)
                    ~label: (`String content)
                    ()
                )
            | _ -> None
        )
        code
      |> Option.some
