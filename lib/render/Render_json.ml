open Forester_prelude
open Forester_core
open Sem

let render_tree ~root ~trees ~dev (doc : Sem.tree) =
  let addr = doc.fm.addr in
  let title =
    match doc.fm.title with
    | None -> `Null
    | Some title ->
      let title = Render_util.expand_title_with_parents ~trees doc.fm title in
      let title_string =
        String.trim @@
        String_util.sentence_case @@
        Render_text.Printer.contents @@
        Render_text.render ~trees title
      in
      `String title_string
  in
  let
    taxon =
    match doc.fm.taxon with
    | None -> `Null
    | Some taxon -> `String (String_util.sentence_case taxon)
  in
  let tags = `List (List.map (fun t -> `String t) doc.fm.tags) in
  let route = `String (Serialise_xml_tree.route ~root addr) in
  let metas =
    let meta_string meta =
      String.trim @@
      String_util.sentence_case @@
      Render_text.Printer.contents @@
      Render_text.render ~trees meta
    in
    `Assoc
      (List.map (fun (s, meta) -> (s, `String (meta_string meta)))
         doc.fm.metas)
  in
  let
    path =
    if dev then
      match doc.fm.source_path with
      | Some p -> [("sourcePath", `String p)]
      | None -> []
    else []
  in
  match addr with
  | User_addr addr ->
    Some
      (addr,
       `Assoc
         ( path @
           [("title", title);
            ("taxon", taxon);
            ("tags", tags);
            ("route",route);
            ("metas", metas);
           ]))
  | _ -> None

let render_trees ~(dev : bool) ~root (trees : Sem.tree Addr_map.t) : Yojson.Basic.t =
  `Assoc begin
    Addr_map.to_seq trees
    |> Seq.map snd
    |> List.of_seq
    |> Sem.Util.sort_for_index
    |> List.filter_map (render_tree ~root ~trees ~dev)
  end

