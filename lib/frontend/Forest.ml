open Eio.Std
open Forester_prelude
open Forester_core
open Forester_render

module M = Addr_map

type config =
  {env : Eio_unix.Stdenv.base;
   assets_dirs : Eio.Fs.dir_ty Eio.Path.t list;
   theme_dir : Eio.Fs.dir_ty Eio.Path.t;
   root : string option;
   stylesheet : string;
   ignore_tex_cache : bool;
   no_assets: bool;
   no_theme: bool}

type raw_forest = Code.tree list

type forest =
  {trees : Sem.tree M.t;
   run_query : Query.dbix Query.expr -> Addr_set.t}

module LaTeX_queue = LaTeX_queue.Make ()

let plant_forest (trees : raw_forest) : forest =
  let module I = struct let enqueue_latex = LaTeX_queue.enqueue end in
  let module Ev = Eval.Make (I) in

  let add_tree addr tree trees =
    if M.mem addr trees then
      begin
        Reporter.emitf Duplicate_tree "skipping duplicate tree at address `%a`" pp_addr addr;
        trees
      end
    else
      M.add addr tree trees
  in

  let unexpanded_trees =
    let alg acc (tree : Code.tree) =
      match tree.addr with
      | Some addr -> add_tree (User_addr addr) tree acc
      | None -> acc
    in
    List.fold_left alg M.empty trees
  in

  let (_, trees) =
    let import_graph = Import_graph.build_import_graph trees in
    let task addr (units, trees) =
      let tree = M.find_opt addr unexpanded_trees in
      match tree with
      | None -> units, trees
      | Some tree ->
        let units, syn = Expand.expand_tree units tree in
        let tree, emitted_trees = Ev.eval_tree ~addr ~source_path:tree.source_path syn in
        let add trees (tree : Sem.tree)  =
          add_tree tree.fm.addr tree trees
        in
        units, List.fold_left add trees @@ tree :: emitted_trees
    in
    Import_graph.Topo.fold task import_graph (Expand.UnitMap.empty, M.empty)
  in

  {trees; run_query = Ev.run_query}

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

let complete ~forest prefix =
  forest.trees
  |> M.filter_map (fun _ -> Sem.Util.peek_title)
  |> M.filter (fun _ -> String.starts_with ~prefix)
  |> M.to_seq
  |> Seq.filter_map (fun (addr, x) -> Addr.to_user_addr addr |> Option.map (fun s -> s, x))

module Prefix_map = Map.Make (String)
let prefixes ~addrs =
  let cat_maybes  =
    List.fold_left
      (fun acc x -> match x with Some x -> x :: acc | None -> acc)
      []
  in
  let split_and_add_to_map map addr =
    let prefix, ix = split_addr addr in
    Prefix_map.add_to_list prefix ix map
  in
  addrs
  |> Seq.fold_left split_and_add_to_map Prefix_map.empty
  |> Prefix_map.map cat_maybes

let taxa ~forest =
  forest.trees
  |> M.filter_map (fun _ -> Sem.Util.taxon)
  |> M.to_seq
  |> Seq.filter_map (fun (addr, x) -> Addr.to_user_addr addr |> Option.map (fun s -> s, x))

let tags ~forest =
  forest.trees
  |> M.map Sem.Util.tags
  |> M.filter (fun _ -> fun tags -> not @@ List.is_empty tags)
  |> M.to_seq
  |> Seq.filter_map (fun (addr, x) -> Addr.to_user_addr addr |> Option.map (fun s -> s, x))


let render_json ~cfg ~cwd docs =
  let root = cfg.root in
  Yojson.Basic.to_file "./output/forest.json" @@
  Render_json.render_trees ~dev:false ~root docs

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

let last_changed env forest scope =
  let (let*) = Option.bind in
  let* tree = M.find_opt scope forest.trees in
  let* source_path = tree.fm.source_path in
  let path = Eio.Path.(Eio.Stdenv.fs env / source_path) in
  let stat  = Eio.Path.stat ~follow:true path in
  let* mtime = Some stat.mtime in
  let* ptime = Ptime.of_float_s mtime in
  let (yyyy, mm, dd) = ptime |> Ptime.to_date_time |> fst in
  Some (Date.{yyyy; mm = Some mm; dd = Some dd})

let render_trees ~cfg ~(forest : forest) ~render_only : unit =
  let env = cfg.env in
  let cwd = Eio.Stdenv.cwd env in

  Eio_util.ensure_dir_path cwd ["output"];
  Eio_util.ensure_dir_path cwd ["build"; "resources"];

  LaTeX_queue.process ~env ~ignore_tex_cache:cfg.ignore_tex_cache;

  let module I =
  struct
    let root, trees, run_query, last_changed, enqueue_latex =
      cfg.root, forest.trees, forest.run_query, last_changed env forest, LaTeX_queue.enqueue

    let get_resource ~hash =
      Eio.Path.load Eio.Path.(cwd / "build" / "resources" / hash)
  end

  in

  let module C = Compile.Make (I) () in
  let module Sxml = Serialise_xml_tree.Make (I) () in

  let render_tree (Xml_tree.Tree tree) =
    let addr = Option.get @@ tree.frontmatter.addr in
    let create = `Or_truncate 0o644 in
    let path = Eio.Path.(cwd / "output" / Serialise_xml_tree.route ~root:cfg.root addr) in
    Eio.Path.with_open_out ~create path @@ fun flow ->
    Eio.Buf_write.with_flow flow @@ fun writer ->
    let fmt = Eio_util.formatter_of_writer writer in
    Xml_tree.Tree tree |> Sxml.pp ~stylesheet:cfg.stylesheet fmt
  in

  let trees =
    match render_only with
    | None -> forest.trees |> M.to_seq |> Seq.map snd |> List.of_seq
    | Some addrs ->
      addrs |> List.map @@ fun addr ->
      match M.find_opt addr forest.trees with
      | Some tree -> tree
      | None ->
        Reporter.fatalf Tree_not_found "Could not find tree with address `%a` when rendering forest" pp_addr addr
  in

  trees |> Sem.Util.sort |> List.map C.compile_tree |> List.iter render_tree;
  render_json ~cfg ~cwd forest.trees;
  if not cfg.no_assets then
    copy_assets ~env ~assets_dirs:cfg.assets_dirs;
  if not cfg.no_theme then
    copy_theme ~env ~theme_dir:cfg.theme_dir