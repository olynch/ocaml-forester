(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

open Forester_prelude
open Forester_core
open Forester_forest

module M = Iri_map
module T = Types
module EP = Eio.Path

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty EP.t

type format = JSON | HTML | XML

let (let*) = Option.bind

let output_dir_name = "output"

let create_tree ~env ~prefix ~template ~mode ~config ~forest =
  let module FU = Forest_util.Make(struct let forest = forest end) in
  let addrs =
    let@ article = List.filter_map @~ Compiler.get_all_articles forest in
    let@ iri = Option.bind article.frontmatter.iri in
    let (Absolute path | Relative path) = Iri.path iri in
    match List.rev path with
    | name :: _ ->
      let* path = article.frontmatter.source_path in
      Some (name, path)
    | _ ->
      None
  in
  let next, next_dir = Iri_util.next_addr addrs ~prefix ~mode ~config in
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
  let path =
    EP.(env#fs / next_dir / fname)
  in
  EP.save
    ~create
    path @@
  body ^ template_content;
  EP.native_exn path

let complete ~forest prefix =
  let module FU = Forest_util.Make(struct let forest = forest end) in
  let module PT = Plain_text_client.Make(struct
    let route = Iri.to_uri
    let forest =
      forest
  end) in
  let config = Compiler.get_config forest in
  let@ article = Seq.filter_map @~ List.to_seq @@ Compiler.get_all_articles forest in
  let@ iri = Option.bind article.frontmatter.iri in
  let@ iri = Option.bind @@ Option_util.guard Iri_scheme.is_named_iri iri in
  let iri = Iri_scheme.relativise_iri ~host: config.host iri in
  let@ title = Option.bind article.frontmatter.title in
  let title = Format.asprintf "%a" PT.pp_content title in
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

let plant_assets ~env ~host ~asset_dirs ~forest =
  (* Logs.debug (fun m -> m "Planting assets: %a" Format.(pp_print_list pp_print_string) (List.map Eio.Path.native_exn asset_dirs)); *)
  let paths = Dir_scanner.scan_directories asset_dirs in
  Logs.debug (fun m -> m "planting %i assets" (Seq.length paths));
  let@ source_path = Seq.iter @~ paths in
  let source_path = String.concat "/" source_path in
  let cwd = Eio.Stdenv.cwd env in
  let content = EP.load EP.(cwd / source_path) in
  let iri = Forester_compiler.Asset_router.install ~host ~source_path ~content in
  Compiler.plant_resource (T.Asset { iri; host; content }) forest

let render_tree ~env ~format ~config addr : unit =
  let@ () = Reporter.silence in
  let forest = Compiler.init ~env ~config in
  (* let tree_dirs = Eio_util.paths_of_dirs ~env config.trees in *)
  let asset_dirs = Eio_util.paths_of_dirs ~env config.assets in
  let config = Compiler.get_config forest in
  let addr = Iri_scheme.user_iri ~host: config.host addr in
  let host = config.host in
  plant_assets ~env ~host ~asset_dirs ~forest;
  (* let router = Iri.of_tree_dirs ~host tree_dirs in *)
  Compiler.(
    forest
    |> build_import_graph_for ~addr
    |> expand_only addr
    |> eval
    |> resources
    |> Fun.flip Forest.find_opt addr
    |> function
    | None -> assert false
    | Some resource ->
      match resource with
      | T.Asset _ -> assert false
      | T.Article article ->
        match format with
        | XML ->
          let module X = Legacy_xml_client.Make(struct let forest = forest end)() in
          let xml = X.render_article forest article in
          Format.printf "%a" (Pure_html.pp_xml ~header: false) xml
        | JSON ->
          Format.printf "%a" (Repr.pp_json T.(article_t content_t)) article
        | HTML ->
          let module H = Htmx_client.Make(struct let forest = forest end)() in
          let html = H.render_article forest article in
          Format.printf "%a\n" Pure_html.pp html
  )

let plant_raw_forest_from_dirs ~env ~(config : Config.Forest_config.t) : Compiler.state =
  let asset_dirs = Eio_util.paths_of_dirs ~env config.assets in
  let tree_dirs = Eio_util.paths_of_dirs ~env config.trees in
  let foreign_paths = Eio_util.paths_of_dirs ~env config.foreign in
  let forest = Compiler.init ~env ~config in
  let host = (Compiler.get_config forest).host in
  begin
    let@ path = List.iter @~ foreign_paths in
    let path_str = EP.native_exn path in
    let@ () = Reporter.profile @@ Format.sprintf "Implant foreign forest from `%s'" path_str in
    let blob = try EP.load path with _ -> Reporter.fatalf IO_error "Could not read foreign forest blob at `%s`" path_str in
    match Repr.of_json_string (T.forest_t T.content_t) blob with
    | Ok foreign_forest ->
      List.iter (fun r -> Compiler.plant_resource r forest) foreign_forest
    | Error (`Msg err) ->
      Reporter.fatalf Parse_error "Could not parse foreign forest blob: %s" err
    | exception (Iri.Error err) ->
      Reporter.fatalf Parse_error "Encountered error while decoding foreign forest blob: %s" (Iri.string_of_error err)
    | exception exn ->
      Reporter.fatalf Parse_error "Encountered unknown error while decoding foreign forest blob: %s" (Printexc.to_string exn)
  end;
  plant_assets ~env ~host ~asset_dirs ~forest;
  Compiler.(
    forest
    |> load tree_dirs
    |> parse ~quit_on_error: true
    |> build_import_graph
    |> expand ~quit_on_error: true
    |> eval
  )

let json_manifest ~dev ~(forest : Compiler.state) : string =
  let config = Compiler.get_config forest in
  let module FU = Forest_util.Make(struct let forest = forest end) in
  let module P = struct let forest = forest end in
  let module Client = Legacy_xml_client.Make(P)() in
  let module R = Json_manifest_client.Make(struct let route = Iri.to_uri let forest = forest end) in
  Yojson.Basic.to_string @@ R.render_trees ~dev ~host: config.host ~forest (Compiler.get_all_articles forest)

let render_forest ~dev ~(forest : Compiler.state) : unit =
  let module P = struct let forest = forest end in
  let module Client = Legacy_xml_client.Make(P)() in
  let module R = Json_manifest_client.Make(struct let route = Iri.to_uri let forest = forest end) in
  let@ () = Reporter.profile "Render forest" in
  let cwd = Eio.Stdenv.cwd (Compiler.get_env forest) in
  let config = Compiler.get_config forest in
  let all_resources = forest |> Compiler.get_all_resources in
  Logs.debug (fun m -> m "rendering %i resources" (List.length all_resources));
  begin
    let all_articles = List.filter_map (function T.Article article -> Some article | _ -> None) all_resources in
    let json_string = Yojson.Basic.to_string @@ R.render_trees ~dev ~host: config.host ~forest all_articles in
    let json_path = EP.(cwd / output_dir_name / "forest.json") in
    Eio_util.ensure_context_of_path ~perm: 0o755 json_path;
    EP.save ~create: (`Or_truncate 0o644) json_path json_string
  end;
  begin
    let@ resource = Eio.Fiber.List.iter ~max_fibers: 20 @~ all_resources in
    let@ () = Reporter.easy_run in
    match resource with
    | T.Article article ->
      let@ route = Option.iter @~ Option.map (fun iri -> Client.route iri forest) article.frontmatter.iri in
      let path = EP.(cwd / output_dir_name / route) in
      Eio_util.ensure_context_of_path ~perm: 0o755 path;
      let@ flow = EP.with_open_out ~create: (`Or_truncate 0o644) path in
      let@ writer = Eio.Buf_write.with_flow flow in
      Client.pp_xml ~stylesheet: "default.xsl" forest (Eio.Buf_write.make_formatter writer) article
    | T.Asset asset ->
      let route = Client.route asset.iri forest in
      let dest = EP.(cwd / output_dir_name / route) in
      if Eio_util.file_exists dest then ()
      else
        begin
          Eio_util.ensure_context_of_path ~perm: 0o755 dest;
          EP.save ~create: (`Or_truncate 0o644) dest asset.content
        end
  end

(* let _export ~forest : unit = *)
(*   let@ () = Reporter.profile "Export forest" in *)
(*   let local_resources = *)
(*     let@ resource = List.filter @~ Compiler.get_all_resources forest in *)
(*     match resource with *)
(*     | T.Article { frontmatter = { iri = Some iri; _ }; _ } -> *)
(*       Iri.host iri = Some forest.config.host *)
(*     | T.Asset asset -> asset.host = forest.config.host *)
(*     | _ -> false *)
(*   in *)
(*   let cwd = Eio.Stdenv.cwd forest.env in *)
(*   let result = Repr.to_json_string ~minify: true (T.forest_t T.content_t) @@ local_resources in *)
(*   let dir = Eio.Path.(cwd / "export") in *)
(*   let filename = forest.config.host ^ ".json" in *)
(*   Eio.Path.mkdirs ~exists_ok: true ~perm: 0o755 dir; *)
(*   Eio.Path.save ~create: (`Or_truncate 0o644) Eio.Path.(dir / filename) result *)
