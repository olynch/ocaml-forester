(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_compiler

module M = Iri_map
module T = Types
module EP = Eio.Path

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty EP.t

type target = Target : 'a Render.target -> target

let (let*) = Option.bind

let output_dir_name = "output"

let create_tree ~env ~dest_dir ~prefix ~template ~mode ~config ~(forest : State.t) =
  let addrs =
    let@ article = List.filter_map @~ Forest.get_all_articles forest.resources in
    let@ iri = Option.bind article.frontmatter.iri in
    let* path = article.frontmatter.source_path in
    Some (iri, path)
  in
  let next, next_dir = Iri_util.next_iri addrs ~prefix ~mode ~config in
  let fname = next ^ ".tree" in
  let now = Human_datetime.now () in
  let template_content =
    match template with
    | None -> ""
    | Some name ->
      EP.load
        EP.(Eio.Stdenv.cwd env / "templates" / (name ^ ".tree"))
  in
  let body = Format.asprintf "\\date{%a}\n" Human_datetime.pp now in
  let create = `Exclusive 0o644 in
  (* If no dest_dir is passed, use the directory of the last previous tree *)
  let dir = match dest_dir with Some dir -> dir | None -> next_dir in
  let path =
    EP.(env#fs / dir / fname)
  in
  EP.save ~create path @@ body ^ template_content;
  EP.native_exn path

let complete ~forest prefix =
  let config = State.config forest in
  let@ article = Seq.filter_map @~ List.to_seq @@ Forest.get_all_articles forest.resources in
  let@ iri = Option.bind article.frontmatter.iri in
  let@ iri = Option.bind @@ Option_util.guard Iri_scheme.is_named_iri iri in
  let iri = Iri_scheme.relativise_iri ~host: config.host iri in
  let@ title = Option.bind article.frontmatter.title in
  let title = Format.asprintf "%a" Render.(pp ~dev: true forest STRING) (Content title) in
  if String.starts_with ~prefix title then
    Some (iri, title)
  else
    None

let is_hidden_file fname =
  String.starts_with ~prefix: "." fname

let copy_contents_of_dir ~env dir =
  Logs.debug (fun m -> m "copying contents of directory %s." (Eio.Path.native_exn dir));
  let cwd = Eio.Stdenv.cwd env in
  let@ fname = List.iter @~ EP.read_dir dir in
  if not @@ is_hidden_file fname then
    let path = EP.(dir / fname) in
    let source = EP.native_exn path in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir: output_dir_name

let render_tree
    : env: env ->
    config: Config.t ->
    target: target ->
    string ->
    unit
  = fun ~env ~config ~target addr ->
    let dev = true in
    let iri = Iri_scheme.user_iri ~host: config.host addr in
    let Target tgt = target in
    let result = State_machine.render_tree ~env ~config ~dev tgt iri in
    Format.printf "%s" result

(* FIXME: deprecate this*)
let compile ~env ~dev ~(config : Config.t) : State.t =
  State_machine.batch_run ~env ~config ~dev

let json_manifest ~dev ~(forest : State.t) : string =
  let render = Render.render ~dev forest JSON in
  forest.resources
  |> Forest.get_all_articles
  |> List.map (fun tree -> render (Article tree))
  |> List.map
    (
      function
      | `Assoc [r, t] -> r, t
      | _ -> assert false
    )
  |> (fun t -> `Assoc t)
  |> Yojson.Safe.to_string

let render_forest ~dev ~(forest : State.t) : unit =
  let cwd = Eio.Stdenv.cwd (State.env forest) in
  let all_resources = forest.resources |> Forest.get_all_resources in
  List.iter (fun t -> Forest.plant_resource t forest.graphs forest.resources) all_resources;
  Logs.debug (fun m -> m "rendering %i resources" (List.length all_resources));
  begin
    let json_string = json_manifest ~dev ~forest in
    let json_path = EP.(cwd / output_dir_name / "forest.json") in
    Eio_util.ensure_context_of_path ~perm: 0o755 json_path;
    EP.save ~create: (`Or_truncate 0o644) json_path json_string
  end;
  let module Graphs = (val State.graphs forest) in
  begin
    let@ resource = Eio.Fiber.List.iter ~max_fibers: 20 @~ all_resources in
    let@ () = Reporter.easy_run in
    match resource with
    | T.Article article ->
      let@ route =
        Option.iter @~
          Option.map
            (fun iri -> Legacy_xml_client.route forest iri)
            article.frontmatter.iri
      in
      let path = EP.(cwd / output_dir_name / route) in
      Eio_util.ensure_context_of_path ~perm: 0o755 path;
      let@ flow = EP.with_open_out ~create: (`Or_truncate 0o644) path in
      let@ writer = Eio.Buf_write.with_flow flow in
      Render.pp_xml ~dev ~stylesheet: "default.xsl" ~forest (Eio.Buf_write.make_formatter writer) article
    | T.Asset asset ->
      let route = Legacy_xml_client.route forest asset.iri in
      let dest = EP.(cwd / output_dir_name / route) in
      if Eio_util.file_exists dest then ()
      else
        begin
          Eio_util.ensure_context_of_path ~perm: 0o755 dest;
          EP.save ~create: (`Or_truncate 0o644) dest asset.content
        end
  end

let export ~(forest : State.t) : unit =
  Reporter.log Format.pp_print_string "Exporting forest";
  let local_resources =
    let@ resource = List.filter @~ Forest.get_all_resources forest.resources in
    match resource with
    | T.Article { frontmatter = { iri = Some iri; _ }; _ } ->
      Iri.host iri = Some forest.config.host
    | T.Asset asset -> asset.host = forest.config.host
    | _ -> false
  in
  let cwd = Eio.Stdenv.cwd forest.env in
  let result = Repr.to_json_string ~minify: true (T.forest_t T.content_t) @@ local_resources in
  let dir = Eio.Path.(cwd / "export") in
  let filename = forest.config.host ^ ".json" in
  Eio.Path.mkdirs ~exists_ok: true ~perm: 0o755 dir;
  Eio.Path.save ~create: (`Or_truncate 0o644) Eio.Path.(dir / filename) result
