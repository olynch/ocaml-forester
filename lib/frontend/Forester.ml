open Forester_prelude
open Forester_core
open Forester_forest
open Forester_compiler

module M = Iri_map
module T = Xml_tree
module F = Forest.Make(Forest_graphs.Make ())
module FU = Forest_util.Make(F)
module PT = Plain_text_client.Make(F)
module C = T.Comparators(PT)
module EP = Eio.Path

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty EP.t

let output_dir_name = "output"

let get_sorted_articles addrs =
  addrs
  |> Vertex_set.to_seq
  |> Seq.filter_map Vertex.iri_of_vertex
  |> Seq.filter_map F.get_article
  |> List.of_seq
  |> List.sort C.compare_article

let get_all_articles () =
  get_sorted_articles @@ F.run_query @@ Query.isect []

let rec random_not_in keys =
  let attempt = Random.int (36 * 36 * 36 * 36 - 1) in
  if List.fold_left (fun x y -> x || y) false (List.map (fun k -> k = attempt) keys) then
    random_not_in keys
  else
    attempt

let split_addr addr =
  (* primitively check for address of form YYYY-MM-DD *)
  let date_regex = Str.regexp {|^[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]$|} in
  if Str.string_match date_regex addr 0 then
    (addr, None)
  else
    match String.rindex_opt addr '-' with
    | Some i ->
      let prefix = String.sub addr 0 i
      and suffix = String.sub addr (i + 1) (String.length addr - i - 1)
      in
      begin
        match BaseN.Base36.int_of_string suffix with
        | Some key -> prefix, Some key
        | None -> addr, None
      end
    | _ -> addr, None

let next_addr ~prefix ~mode (addrs : string list) =
  let keys =
    let@ addr = List.filter_map @~ addrs in
    let prefix', key = split_addr addr in
    if prefix = prefix' then key else None
  in
  let next =
    match mode with
    | `Sequential -> 1 + List.fold_left max 0 keys
    | `Random -> random_not_in keys
  in
  prefix ^ "-" ^ BaseN.Base36.string_of_int next

let create_tree ~env ~dest ~prefix ~template ~mode =
  let addrs =
    let@ article = List.filter_map @~ get_all_articles () in
    let@ iri = Option.bind article.frontmatter.iri in
    let (Absolute path | Relative path) = Iri.path iri in
    match List.rev path with
    | name :: _ -> Some name
    | _ -> None
  in
  let next = next_addr addrs ~prefix ~mode in
  let fname = next ^ ".tree" in
  let now = Date.now () in
  let template_content =
    match template with
    | None -> ""
    | Some name -> EP.load EP.(Eio.Stdenv.cwd env / "templates" / (name ^ ".tree"))
  in
  let body = Format.asprintf "\\date{%a}\n" Date.pp now in
  let create = `Exclusive 0o644 in
  let path = EP.(dest / fname) in
  EP.save ~create path @@ body ^ template_content;
  next

let complete ~host prefix =
  let@ article = Seq.filter_map @~ List.to_seq @@ get_all_articles () in
  let@ iri = Option.bind article.frontmatter.iri in
  let@ iri = Option.bind @@ Option_util.guard Iri_scheme.is_stable_iri iri in
  let iri = Iri_scheme.relativise_iri ~host iri in
  let@ title = Option.bind article.frontmatter.title in
  let title = Format.asprintf "%a" PT.pp_content title in
  if String.starts_with ~prefix title then
    Some (iri, title)
  else
    None

let is_hidden_file fname =
  String.starts_with ~prefix: "." fname

let copy_contents_of_dir ~env dir =
  let cwd = Eio.Stdenv.cwd env in
  let@ fname = List.iter @~ EP.read_dir dir in
  if not @@ is_hidden_file fname then
    let path = EP.(dir / fname) in
    let source = EP.native_exn path in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir: output_dir_name

let parse_trees_in_dirs ~dev ?(ignore_malformed = false) dirs =
  let@ () = Reporter.profile "Parse trees" in
  let@ fp = List.filter_map @~ List.of_seq @@ Forest_scanner.scan_directories dirs in
  let@ _, basename = Option.bind (EP.split fp) in
  let addr = Filename.chop_extension basename in
  let native = EP.native_exn fp in
  let source_path = if dev then Some (Unix.realpath native) else None in
  match Parse.parse_file native with
  | Result.Ok code -> Some Code.{ source_path; addr = Some addr; code }
  | Result.Error _ -> None
  | exception exn ->
    if ignore_malformed then None else raise exn

let plant_raw_forest_from_dirs ~env ~host ~dev ~tree_dirs ~asset_dirs ~foreign_dirs : unit =
  let parsed_trees = parse_trees_in_dirs ~dev tree_dirs in
  begin
    let@ () = Reporter.profile "Read assets" in
    let@ source_path = Seq.iter @~ Dir_scanner.scan_directories asset_dirs in
    let source_path = String.concat "/" source_path in
    let cwd = Eio.Stdenv.cwd env in
    let content = EP.load EP.(cwd / source_path) in
    let filename, iri = Forester_compiler.Asset_content_addresser.install ~source_path ~content in
    F.plant_resource @@ F.Asset { iri; contents = content; filename }
  end;
  begin
    let@ () = Reporter.profile "Expand, evaluate, and analyse forest" in
    begin
      let@ _, article = Seq.iter @~ Iri_map.to_seq @@ Forest_reader.read_trees ~host ~env parsed_trees in
      F.plant_resource @@ F.Article article
    end;
    begin
      let@ _foreign_dir = List.iter @~ foreign_dirs in
      (* TODO: plant the trees & assets from the foreign directory *)
      ()
    end
  end

let json_manifest ~host ~home ~dev : string =
  let all_articles = FU.get_all_articles () in
  let module P = struct let host = host let home = home end in
  let module Client = Legacy_xml_client.Make(P)(F)() in
  let module R = Json_manifest_client.Make(Client)(F) in
  Yojson.Basic.to_string @@ R.render_trees ~dev ~host all_articles

let render_forest ~env ~dev ~host ~home : unit =
  let module P = struct let host = host let home = home end in
  let module Client = Legacy_xml_client.Make(P)(F)() in
  let module R = Json_manifest_client.Make(Client)(F) in
  let@ () = Reporter.profile "Render forest" in
  let cwd = Eio.Stdenv.cwd env in
  let all_resources = List.of_seq @@ F.get_all_resources () in
  begin
    let all_articles = List.filter_map (function F.Article article -> Some article | _ -> None) all_resources in
    let json_string = Yojson.Basic.to_string @@ R.render_trees ~dev ~host all_articles in
    let json_path = EP.(cwd / output_dir_name / "forest.json") in
    Eio_util.ensure_context_of_path ~perm: 0o755 json_path;
    EP.save ~create: (`Or_truncate 0o644) json_path json_string
  end;
  begin
    let@ resource = Eio.Fiber.List.iter ~max_fibers: 20 @~ all_resources in
    let@ () = Reporter.easy_run in
    match resource with
    | F.Article article ->
      let@ route = Option.iter @~ Option.map Client.route article.frontmatter.iri in
      let path = EP.(cwd / output_dir_name / route) in
      Eio_util.ensure_context_of_path ~perm: 0o755 path;
      let@ flow = EP.with_open_out ~create: (`Or_truncate 0o644) path in
      let@ writer = Eio.Buf_write.with_flow flow in
      Client.pp_xml ~stylesheet:"default.xsl" (Eio.Buf_write.make_formatter writer) article
    | F.Asset asset ->
      let route = Client.route asset.iri in
      let dest = EP.(cwd / output_dir_name / route) in
      if Eio_util.file_exists dest then ()
      else
        begin
          Eio_util.ensure_context_of_path ~perm: 0o755 dest;
          EP.save ~create: (`Or_truncate 0o644) dest asset.contents
        end
  end

let export ~env ~host ~asset_dirs : unit =
  let@ () = Reporter.profile "Export forest" in
  let cwd = Eio.Stdenv.cwd env in
  let trees = FU.get_all_articles () in
  let result = Json_client.render_trees ~host trees in
  let dir = Eio.Path.(cwd / "export" / host) in
  Eio.Path.mkdirs ~exists_ok: true ~perm: 0o755 dir;
  Eio.Path.save ~create: (`Or_truncate 0o644) Eio.Path.(dir / "forest.json") result;
  let@ dir_to_copy = List.iter @~ asset_dirs in
  let source = EP.native_exn dir_to_copy in
  let dest_dir = EP.native_exn dir in
  Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir
