open Repr
open Base
open Xml_tree

let string_source : Range.string_source t =
  let open Range in
  record "string_source" (fun title content -> { title; content })
  |+ field "title" (option string) (fun s -> s.title)
  |+ field "content" string (fun s -> s.content)
  |> sealr

let source_repr : Range.source ty =
  let open Range in
  variant "source" (fun file string -> function
    | `File s -> file s | `String s -> string s)
  |~ case1 "File" string (fun s -> `File s)
  |~ case1 "String" string_source (fun s -> `String s)
  |> sealv

let position : Range.position ty =
  let open Range in
  record "position" (fun source offset start_of_line line_num ->
      { source; offset; start_of_line; line_num })
  |+ field "source" source_repr (fun t -> t.source)
  |+ field "offset" int (fun t -> t.offset)
  |+ field "start_of_line" int (fun t -> t.offset)
  |+ field "line_num" int (fun t -> t.offset)
  |> sealr

let range : Range.t ty =
  (* NOTE:
     For the irmin-git backend, the functions we need are pp, of_string and
     equal. I've worked around the need for a full of_string implementation
     (parser), since in `located_sem_node` I am simply returning `None` for the
     value of `loc`. This means even though we can serialize ranges, we can't
     retrieve them. This is fine for now, since we don't need that info for
     rendering, which is our primary use case.
  *)
  let open Range in
  let pp = Range.dump in
  let pos =
    { source = `File "todo"; offset = 0; start_of_line = 0; line_num = 0 }
  in

  let of_string str =
    (* HACK: Should parse this kind of string (produced by Range.dump):

       Range
       ({source=(`File "todo"); offset=0; start_of_line=0; line_num=0},
        {source=(`File "todo"); offset=0; start_of_line=0; line_num=0})
    *)
    Ok (Range.make (pos, pos))
  in

  let r = Range.make (pos, pos) in
  let encode encoder range = () in
  let decode _ = Ok r in
  let encode_bin : _ encode_bin = fun _ _ -> () in
  let decode_bin _ _ = r in
  let size_of : _ size_of =
    (* NOTE: Named args of_value and of_encoding are optional.
       Precompute the size that will be used by `encode_bin`. `of_encoding`
       unused nowadays
    *)
    Size.custom_dynamic ()
  in

  let compare_pos p q =
    p.source = q.source && p.offset = q.offset
    && p.start_of_line = q.start_of_line
    && p.line_num = q.line_num
  in

  let equal r1 r2 =
    match (Range.view r1, Range.view r2) with
    | `End_of_file p, `End_of_file q -> compare_pos p q
    | `Range (p1, p2), `Range (q1, q2) -> compare_pos p1 q1 && compare_pos p2 q2
    | _ -> false
  in
  let compare r1 r2 =
    if equal r1 r2 then 0
    else
      (*  FIXME: Is this used by the git-backend? If not, remove it.
      *)
      match (Range.view r1, Range.view r2) with
      | `End_of_file p, `End_of_file q ->
          if p.source = q.source then
            match (p.source, q.source) with
            | `String s1, `String s2 -> String.compare s1.content s2.content
            | `File s1, `File s2 -> String.compare s1 s2
            | _ -> -1
          else -1
      | `Range (p1, p2), `Range (q1, q2) -> -1
      | _ -> -1
  in
  let short_hash ?seed a = 0 in
  let pre_hash _ _ = () in
  abstract ~pp ~of_string ~json:(encode, decode)
    ~bin:(encode_bin, decode_bin, size_of)
    ~equal ~compare ~short_hash ~pre_hash ()

let expr_t var_t =
  let open Repr in
  let open Query in
  mu (fun expr_t ->
      variant "expr" (fun rel isect union complement union_fam isect_fam ->
          function
        | Rel (x1, x2, x3, x4) -> rel (x1, x2, x3, x4)
        | Isect x -> isect x
        | Union x -> union x
        | Complement x -> complement x
        | Union_fam (x, y) -> union_fam (x, y)
        | Isect_fam (x, y) -> isect_fam (x, y))
      |~ case1 "Rel"
           (quad mode_t polarity_t Rel.t (addr_expr_t var_t))
           (fun (x1, x2, x3, x4) -> Rel (x1, x2, x3, x4))
      |~ case1 "Isect" (list expr_t) (fun x -> Isect x)
      |~ case1 "Union" (list expr_t) (fun x -> Union x)
      |~ case1 "Complement" expr_t (fun x -> Complement x)
      |~ case1 "Union_fam"
           (pair expr_t (binder_t expr_t))
           (fun (x, y) -> Union_fam (x, y))
      |~ case1 "Isect_fam"
           (pair expr_t (binder_t expr_t))
           (fun (x, y) -> Isect_fam (x, y))
      |> sealv)

(* https://github.com/mirage/repr/issues/46,
 *)
let content_node_t resource_t : content_node Repr.ty =
  let open Repr in
  mu (fun content_node_t ->
      variant "content_node"
        (fun
          text
          cdata
          xml_elt
          transclude
          results_of_query
          section
          prim
          katex
          tex_cs
          link
          img
          resource
        -> function
        | Text x -> text x
        | CDATA x -> cdata x
        | Xml_elt x -> xml_elt x
        | Transclude x -> transclude x
        | Results_of_query x -> results_of_query x
        | Section x -> section x
        | Prim (x, y) -> prim (x, y)
        | KaTeX (x, y) -> katex (x, y)
        | TeX_cs x -> tex_cs x
        | Link x -> link x
        | Img x -> img x
        | Resource x -> resource x)
      |~ case1 "Text" string (fun x -> Text x)
      |~ case1 "CDATA" string (fun x -> CDATA x)
      |~ case1 "Xml_elt" (xml_elt_t (list content_node_t)) (fun x -> Xml_elt x)
      |~ case1 "Transclude"
           (transclusion_t (list content_node_t))
           (fun x -> Transclude x)
      |~ case1 "Results_of_query" (expr_t Query.dbix_t) (fun x ->
             Results_of_query x)
      |~ case1 "Section" (section_t (list content_node_t)) (fun x -> Section x)
      |~ case1 "Prim"
           (pair Prim.t (list content_node_t))
           (fun (x, y) -> Prim (x, y))
      |~ case1 "KaTeX"
           (pair math_mode_t (list content_node_t))
           (fun (x, y) -> KaTeX (x, y))
      |~ case1 "TeX_cs" TeX_cs.t (fun x -> TeX_cs x)
      |~ case1 "Link" (link_t (list content_node_t)) (fun x -> Link x)
      |~ case1 "Img" img_t (fun x -> Img x)
      |~ case1 "Resource" resource_t (fun x -> Resource x)
      |> sealv)

let resource_t content_node_t : resource Repr.ty =
  let open Repr in
  record "resource" (fun hash content sources -> { hash; content; sources })
  |+ field "hash" string (fun x -> x.hash)
  |+ field "content" (list content_node_t) (fun x -> x.content)
  |+ field "sources" (list resource_source_t) (fun x -> x.sources)
  |> sealr

let resource, content_node =
  Repr.mu2 (fun r z -> (resource_t z, content_node_t r))

let content_t =
  let open Repr in
  list content_node
