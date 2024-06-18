open Forester_core

let rec expand_title_with_parents ~trees ?(ancestors = []) (fm : Sem.frontmatter) title =
  match fm.designated_parent with
  | Some parent_addr when not @@ List.mem parent_addr ancestors ->
    begin
      match Addr_map.find_opt parent_addr trees with
      | None ->
        title
      | Some (parent : Sem.tree)  ->
        let chevron = [Range.locate_opt None @@ Sem.Text " › "] in
        let parent_title =
          parent.fm.title |>
          Option.map @@ expand_title_with_parents ~trees parent.fm
        in
        let parent_link =
          [Range.locate_opt None @@
           Sem.Link (parent_addr, parent_title,  Sentence_case)]
        in
        parent_link @ chevron @ title
    end
  | _ -> title
