open Forester_compiler
module L = Lsp.Types

let kind
    : Syn.node -> L.CompletionItemKind.t option
  = function
  | Fun (_, _) -> Some Function
  | Text _ | Verbatim _ -> Some Text
  | Meta -> Some Field
  | Route_asset -> Some File
  | Var _ -> Some Variable
  | Prim _
  | Transclude
  | Embed_tex
  | Ref
  | Title
  | Parent
  | Taxon
  | Attribution (_, _)
  | Tag _
  | Date
  | Number ->
    Some Keyword
  | Group (_, _)
  | Math (_, _)
  | Link _
  | Subtree (_, _)
  | Sym _
  | Put (_, _, _)
  | Default (_, _, _)
  | Get _
  | Xml_tag (_, _, _)
  | TeX_cs _
  | Object _
  | Patch _
  | Call (_, _)
  | Query_polarity _
  | Query_mode _
  | Results_of_query
  | Query_rel _
  | Query_isect
  | Query_union
  | Query_compl
  | Query_isect_fam
  | Query_union_fam
  | Query_isect_fam_rel
  | Query_union_fam_rel
  | Query_builtin (_, _)
  | Dx_sequent (_, _)
  | Dx_query (_, _, _)
  | Dx_prop (_, _)
  | Dx_var _
  | Dx_const (_, _)
  | Dx_execute
  | Publish_results_of_query ->
    None

let insert_text path = String.concat "/" path

let make
    : Yuujinchou.Trie.path
    * (Resolver.P.data * unit) ->
    L.CompletionItem.t option
  = fun (path, (data, _)) ->
    match data with
    | Resolver.P.Term syn ->
      (* NOTE: Eventually we want to analyse the syntax so that, for example,
         you can tab through the snippet for a function of arity n*)
      let kind = kind (List.hd syn).value in
      let insertText = insert_text path in
      Some
        (
          L.CompletionItem.create
            ?kind
            ~insertText
            ~label: (String.concat "/" path)
            ()
        )
    | Resolver.P.Xmlns _ ->
      None
