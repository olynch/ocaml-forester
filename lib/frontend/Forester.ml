open Forester_prelude
open Forester_core
open Forester_forest
open Forester_compiler

module M = Addr_map
module T = Xml_tree
module F = Forest.Make (Forest_graphs.Make ())
module FU = Forest_util.Make (F)
module PT = Plain_text_client.Make (F)
module C = T.Comparators (PT)
module EP = Eio.Path

type env = Eio_unix.Stdenv.base
type dir = Eio.Fs.dir_ty EP.t

let output_dir_name = "output"

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
      and suffix = String.sub addr (i + 1) (String.length addr - i - 1) in
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
    match article.frontmatter.addr with
    | Addr.User_addr x -> Some x
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

let complete prefix =
  let@ article = Seq.filter_map @~ List.to_seq @@ get_all_articles () in
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

let copy_contents_of_dir ~env dir =
  let cwd = Eio.Stdenv.cwd env in
  let@ fname = List.iter @~ EP.read_dir dir in
  if not @@ is_hidden_file fname then
    let path = EP.(dir / fname) in
    let source = EP.native_exn path in
    Eio_util.copy_to_dir ~env ~cwd ~source ~dest_dir:output_dir_name

let parse_trees_in_dirs ~dev ?(ignore_malformed = false) dirs =
  let@ () = Reporter.profile "parse trees" in
  let@ fp = List.filter_map @~ List.of_seq @@ Forest_scanner.scan_directories dirs in
  let@ _, basename = Option.bind (EP.split fp) in
  let addr = Filename.chop_extension basename in
  let native = EP.native_exn fp in
  let source_path = if dev then Some (Unix.realpath native) else None in
  match Parse.parse_file native with
  | Result.Ok code -> Some Code.{source_path; addr = Some addr; code}
  | Result.Error _ -> None
  | exception exn ->
    if ignore_malformed then None else raise exn

let plant_forest_from_dirs ~env ~dev tree_dirs : unit =
  let parsed_trees = parse_trees_in_dirs ~dev tree_dirs in
  let@ () = Reporter.profile "expand, evaluate, and analyse forest" in
  Forest_reader.read_trees ~env parsed_trees
  |> Addr_map.iter (fun _ -> F.plant_article)


let json_manifest ~root ~dev : string =
  let all_articles = FU.get_all_articles () in
  let module P = struct let root = root end in
  let module Client = Legacy_xml_client.Make (P) (F) () in
  let module R = Render_json.Make (Client) (F) in
  Yojson.Basic.to_string @@ R.render_trees ~dev all_articles

let render_forest ~env ~dev ~root ~stylesheet : unit =
  let@ () = Reporter.profile "render forest" in
  let cwd = Eio.Stdenv.cwd env in
  Eio_util.ensure_dir_path cwd [output_dir_name];

  let module P = struct let root = root end in
  let module Client = Legacy_xml_client.Make (P) (F) () in
  let module R = Render_json.Make (Client) (F) in

  let all_articles = FU.get_all_articles () in

  begin
    let@ flow = EP.with_open_out ~create:(`Or_truncate 0o644) EP.(cwd / output_dir_name / "forest.json") in
    let@ writer = Eio.Buf_write.with_flow flow in
    Yojson.Basic.pp (Eio_util.formatter_of_writer writer) @@ R.render_trees ~dev all_articles
  end;

  begin
    let@ article = List.iter @~ all_articles in
    let@ route = Option.iter @~ Client.route article.frontmatter.addr in
    let@ flow = EP.with_open_out ~create:(`Or_truncate 0o644) EP.(cwd / output_dir_name / route) in
    let@ writer = Eio.Buf_write.with_flow flow in
    Client.pp_xml ~stylesheet (Eio_util.formatter_of_writer writer) article
  end
