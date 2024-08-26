open Forester_prelude
open Forester_core

module T = Xml_tree

module Make (R : sig val route : addr -> string option end) (F : Forest.S) =
struct

  module PT = Plain_text_client.Make (F)

  let render_tree ~dev (doc : T.content T.article) =
    let addr = doc.frontmatter.addr in
    let@ route = Option.bind @@ R.route addr in
    let title_string = String_util.sentence_case @@ PT.string_of_content @@ F.get_expanded_title doc.frontmatter in
    let title = `String title_string in
    let taxon =
      match doc.frontmatter.taxon with
      | None -> `Null
      | Some taxon -> `String (String_util.sentence_case taxon)
    in
    let tags = `List (List.map (fun t -> `String t) doc.frontmatter.tags) in
    let route = `String route in
    let metas =
      let meta_string meta = String.trim @@ PT.string_of_content meta in
      `Assoc
        (List.map (fun (s, meta) -> (s, `String (meta_string meta)))
           doc.frontmatter.metas)
    in
    let
      path =
      if dev then
        match doc.frontmatter.source_path with
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

  let render_trees ~(dev : bool) (trees : T.content T.article list) : Yojson.Basic.t =
    `Assoc begin
      trees |> List.filter_map (render_tree ~dev)
    end


end
