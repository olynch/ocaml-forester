let keys =
  let open Bibtex.Field_types in
  let open Bibtex.Fields in
  let str named_field = ( named_field.f @: named_field.conv ) in
  let (|++) database named_field =
    Bibtex.Database.add named_field.name (str named_field) database in
  Bibtex.Fields.default_keys
         |++ str_field ~name:"issn"
         |++ str_field ~name:"publisher"
         (* we don't care about parsing pages as a tuple of numbers *)
         |++ str_field ~name:"pages"

let format_tree b source =
  let open Bibtex.Fields in
  let buf = Buffer.create 1000 in
  (b.%{ title.f } |> Option.iter @@ fun t -> Printf.bprintf buf "\\title{%s}\n" t);
  (b.%{ year.f } |> Option.iter @@ fun y -> Printf.bprintf buf "\\date{%d}\n" y);
  (b.%{ authors.f }
    |> Option.iter @@
    List.iter @@
    fun a ->
      Printf.bprintf buf "\\author{%s-%s}\n" (String.uncapitalize_ascii a.firstname) (String.uncapitalize_ascii a.lastname)
  );
  Printf.bprintf buf "\\taxon{reference}\n";
  (b.%{ doi.f } |> Option.iter @@ fun d -> Printf.bprintf buf "\\meta{doi}{%s}\n" (doi.conv.to_ d));
  Printf.bprintf buf "\\meta{bibtex}{\\verb>>|\n%s\n>>}\n" (String.trim source);
  Buffer.contents buf

let boring_words = ["the"; "a"; "an"; "on"]

let tree_name b =
  let open Bibtex.Fields in
  let author = match b.%{authors.f} with
    | Some (a :: _) -> (String.uncapitalize_ascii a.lastname)
    | _ -> "noname" in
  let title = match b.%{title.f} with
    | Some t ->
       (let words = String.split_on_char ' ' t |> List.map String.uncapitalize_ascii in
       let first_good = List.find_opt (fun w -> not (List.mem w boring_words)) words in
       match first_good with
       | Some w -> w
       | None -> t)
    | None -> "" in
  let year = match b.%{year.f} with
    | Some y -> Printf.sprintf "%d" y
    | None -> "noyear"
  in
  Printf.sprintf "%s-%s-%s" author year title
