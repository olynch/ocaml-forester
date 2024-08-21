open Forester_prelude
open Forester_core
open Forester_forest

module M = Addr_map

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   theme_dir : Eio.Fs.dir_ty Eio.Path.t;
   root : string option;
   stylesheet : string;
   no_assets: bool;
   no_theme: bool}

module T = Xml_tree
module F = Forest.Make (Forest_graphs.Make ())
module PT = Plain_text_client.Make (F)
module C = T.Comparators (PT)

let get_sorted_articles addrs =
  addrs
  |> Addr_set.to_seq
  |> Seq.filter_map F.get_article
  |> List.of_seq
  |> List.sort C.compare_article

let get_all_articles () =
  get_sorted_articles @@ F.run_query @@ Query.isect []

let rec random_not_in keys =
  let attempt = Random.int (36*36*36*36 - 1) in
  if Seq.fold_left (fun x y -> x || y) false (Seq.map (fun k -> k = attempt) keys) then
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
      and suffix = String.sub addr (i + 1) (String.length addr - i - 1) in
      begin
        match BaseN.Base36.int_of_string suffix with
        | Some key -> prefix, Some key
        | None -> addr, None
      end
    | _ -> addr, None

let next_addr ~prefix ~mode (forest : string Seq.t) =
  let keys =
    forest |> Seq.filter_map @@ fun addr ->
    let prefix', key = split_addr addr in
    if prefix = prefix' then key else None
  in
  let next =
    match mode with
    | `Sequential -> 1 + Seq.fold_left max 0 keys
    | `Random -> random_not_in keys
  in
  prefix ^ "-" ^ BaseN.Base36.string_of_int next

let create_tree ~cfg ~addrs ~dest ~prefix ~template ~mode =
  let next = next_addr addrs ~prefix ~mode in
  let fname = next ^ ".tree" in
  let now = Date.now () in
  let template_content =
    match template with
    | None -> ""
    | Some name -> Eio.Path.load Eio.Path.(Eio.Stdenv.cwd cfg.env / "templates" / (name ^ ".tree"))
  in
  let body = Format.asprintf "\\date{%a}\n" Date.pp now in
  let create = `Exclusive 0o644 in
  let path = Eio.Path.(dest / fname) in
  Eio.Path.save ~create path @@ body ^ template_content;
  next

let complete prefix =
  get_all_articles () |> List.to_seq |> Seq.filter_map @@ fun (article : _ T.article) ->
  let addr = article.frontmatter.addr in
  let title =
    Format.asprintf "%a"
      PT.pp_content article.frontmatter.title
  in
  if Addr.is_user_addr addr && String.starts_with ~prefix title then
    Some (addr, title)
  else
    None

let is_hidden_file fname =
  String.starts_with ~prefix:"." fname

let copy_theme ~env ~theme_dir =
  let cwd = Eio.Stdenv.cwd env in
  Eio.Path.read_dir theme_dir |> List.iter @@ fun fname ->
  if not @@ is_hidden_file fname then
    Eio.Path.native @@ Eio.Path.(theme_dir / fname) |> Option.iter @@ fun source ->
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"output"

let copy_assets ~env ~assets_dirs =
  let cwd = Eio.Stdenv.cwd env in
  assets_dirs |> List.iter @@ fun assets_dir ->
  Eio.Path.read_dir assets_dir |> List.iter @@ fun fname ->
  if not @@ is_hidden_file fname then
    let path = Eio.Path.(assets_dir / fname) in
    let source = Eio.Path.native_exn path in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:"output"


let read_and_render_forest ~cfg tree_dirs : unit =
  let parsed_trees =
    Reporter.profile "parse trees" @@ fun () ->
    Process.read_trees_in_dirs ~dev:true tree_dirs
  in

  let articles =
    Reporter.profile "expand, evaluate, and analyse forest" @@ fun () ->
    Forest_reader.read_trees ~env:cfg.env parsed_trees |> Addr_map.map @@ fun article ->
    F.plant_article article;
    article
  in

  begin
    Reporter.profile "render forest" @@ fun () ->
    let cwd = Eio.Stdenv.cwd cfg.env in
    Eio_util.ensure_dir_path cwd ["output"];

    let module Util = Forest_util.Make (F) in
    let module Client = Legacy_xml_client.Make (struct let root = cfg.root end) (F) () in
    let module R = Render_json.Make (Client) (F) in

    let all_articles = Util.get_all_articles () in

    begin
      all_articles |> List.iter @@ fun article ->
      let xml =
        Format.asprintf "%a"
          (Client.pp_xml ~stylesheet:cfg.stylesheet)
          article
      in
      Client.route article.frontmatter.addr |> Option.iter @@ fun route ->
      Eio.Path.save
        ~create:(`Or_truncate 0o644)
        Eio.Path.(cwd / "output" / route) xml
    end;

    Yojson.Basic.to_file "./output/forest.json" @@
    R.render_trees ~dev:false all_articles;
  end;


  if not cfg.no_assets then
    begin
      Reporter.profile "copying assets" @@ fun () ->
      copy_assets ~env:cfg.env ~assets_dirs:cfg.assets_dirs;
    end;
  if not cfg.no_theme then
    begin
      Reporter.profile "copying theme" @@ fun () ->
      copy_theme ~env:cfg.env ~theme_dir:cfg.theme_dir
    end