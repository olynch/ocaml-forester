(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_core

exception Todo of string

module T = Types
module P = Pure_html

type article = T.content T.article
type content = T.content
type frontmatter = T.content T.frontmatter

type _ renderable =
  | Content : content -> content renderable
  | Article : article -> article renderable
  | Frontmatter : frontmatter -> frontmatter renderable

type xml = P.node
type json = Yojson.Safe.t
type html = P.node

type _ target =
  | XML : xml target
  | JSON : json target
  | HTML : html target

module type S = sig
  val render : 'a 'b. 'a renderable -> 'b target -> 'b
end

module Client (S: S) = struct
  let render = S.render
end

module Renderer (F: sig val forest : Compiler.state end) : S = struct
  let forest = F.forest
  let config = Compiler.get_config forest
  let host = config.host

  let render
      : type a b. a renderable -> b target -> b
    = fun
        renderable
        tgt
      ->
      match tgt with
      | JSON ->
        begin
          match renderable with
          | Content content ->
            `String
              (Plain_text_client.string_of_content content)
          | Article article ->
            begin
              match (Json_manifest_client.render_tree ~dev: true ~host article) with
              | Some (r, t) -> `Assoc [r, t]
              | None -> `Null
            end
          | Frontmatter _ -> raise (Todo "")
        end
      | HTML ->
        begin
          match renderable with
          | Content content -> P.HTML.div [] @@ Htmx_client.render_content forest content
          | Article _ -> P.HTML.null []
          | Frontmatter _ -> P.HTML.null []
        end
      (* P.HTML.null [] *)
      | XML -> P.HTML.null []
end
