(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core

exception Todo of string

module T = Types
module P = Pure_html

type _ renderable =
  | Content : T.content -> T.content renderable
  | Article : T.content T.article -> T.content T.article renderable
  | Frontmatter : T.content T.frontmatter -> T.content T.frontmatter renderable

type xml = P.node
type json = Yojson.Safe.t
type html = P.node

type _ target =
  | XML : xml target
  | JSON : json target
  | HTML : html target
  | STRING : string target

let render
    : type a b. dev: bool -> Forester_forest.State.t -> a target -> b renderable -> a
  = fun
      ~dev
      forest
      tgt
      renderable
    ->
    let renderable =
      match renderable with
      | Content _ -> renderable
      | Article a ->
        if dev then
          renderable
        else
          Article { a with frontmatter = { a.frontmatter with source_path = None } }
      | Frontmatter fm ->
        if dev then
          renderable
        else
          Frontmatter { fm with source_path = None }
    in
    match tgt with
    | JSON ->
      begin
        match renderable with
        | Content content ->
          `String
            (Plain_text_client.string_of_content forest content)
        | Article article ->
          begin
            match (Json_manifest_client.render_tree ~dev ~forest article) with
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
    | XML ->
      begin
        match renderable with
        | Content content ->
          P.HTML.null @@ Legacy_xml_client.render_content forest content
        | Article article ->
          Legacy_xml_client.render_article forest article
        | Frontmatter _ ->
          raise (Todo "render frontmatter to xml")
      end
    | STRING ->
      begin
        match renderable with
        | Content content -> Plain_text_client.string_of_content forest content
        | Article _ -> raise (Todo "render article to string")
        | Frontmatter _ -> raise (Todo "render frontmatter to string")
      end

let pp
    : type a b. dev: bool -> Compiler.state -> a target -> Format.formatter -> b renderable -> unit
  = fun ~dev forest target fmt renderable ->
    let stuff =
      render ~dev forest target renderable
    in
    match target with
    | XML -> P.pp_xml fmt stuff
    | HTML -> P.pp fmt stuff
    | JSON -> Yojson.Safe.pp fmt stuff
    | STRING -> Format.pp_print_string fmt stuff

let pp_xml ?stylesheet ~forest ~dev fmt article =
  Format.fprintf fmt {|<?xml version="1.0" encoding="UTF-8"?>|};
  Format.pp_print_newline fmt ();
  begin
    let@ uri = Option.iter @~ stylesheet in
    Format.fprintf fmt "<?xml-stylesheet type=\"text/xsl\" href=\"%s\"?>" uri
  end;
  Format.pp_print_newline fmt ();
  P.pp_xml fmt @@ render ~dev forest XML (Article article)
