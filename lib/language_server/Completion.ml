(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_forest
open Forester_frontend

module L = Lsp.Types

let (let*) = Option.bind

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
    * (Resolver.P.data * Asai.Range.t option) ->
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

let compute
    (params : L.CompletionParams.t)
  =
  match params with
  | {
    context;
    _;
  } ->
    let triggerCharacter =
      match context with
      | Some { triggerCharacter; _ } ->
        triggerCharacter
      | None -> None
    in
    let Lsp_state.{ forest; _ } = Lsp_state.get () in
    let config = Compiler.get_config forest in
    let module PT = Plain_text_client.Make(struct
      let route = Iri.to_uri
      let forest = forest
    end) in
    let addr_items () =
      forest
      |> Compiler.parsed
      |> Forest.to_seq_values
      |> Seq.filter_map
        (
          fun (tree : Code.tree) ->
            let* addr = tree.addr in
            let* { frontmatter; mainmatter; _ } =
              (Compiler.get_article (Iri_scheme.user_iri ~host: config.host addr) forest)
            in
            let documentation =
              let render = PT.string_of_content in
              let title = frontmatter.title in
              let taxon = frontmatter.taxon in
              let content =
                Format.asprintf
                  {|%s\n %s\n %s\n |}
                  (Option.fold ~none: "" ~some: (fun s -> Format.asprintf "# %s" (render s)) title)
                  (Option.fold ~none: "" ~some: (fun s -> Format.asprintf "taxon: %s" (render s)) taxon)
                  (render mainmatter)
              in
              Some (`String content)
            in
            let insertText =
              match triggerCharacter with
              | Some "{" -> addr ^ "}"
              | Some "(" -> addr ^ ")"
              | Some "[" -> addr ^ "]"
              | _ -> addr
            in
            Some (L.CompletionItem.create ?documentation ~label: addr ~insertText ())
        )
      |> List.of_seq
    in
    let scope_items () =
      (* TODO: If the selected item is not in scope in the current tree, auto-import it.
         reference: https://github.com/rust-lang/rust-analyzer/blob/fc98e0657abf3ce07eed513e38274c89bbb2f8ad/crates/ide-assists/src/handlers/auto_import.rs#L15
         *)
      let units = Compiler.units forest in
      units
      |> Expand.Unit_map.to_list
      |> List.map snd
      |> List.concat_map
        (
          fun trie ->
            let open Yuujinchou in
            trie
            |> Trie.to_seq
            |> List.of_seq
            |> List.filter_map make
        )
      |> List.append
        (
          Expand.builtins
          |> List.map (fun (a, b) -> (a, (Resolver.P.Term [Range.locate_opt None b], None)))
          |> List.filter_map make
        )
    in
    let items =
      match triggerCharacter with
      | Some "(" -> addr_items ()
      | Some "{" -> addr_items ()
      | Some "\\" -> scope_items ()
      | _ -> []
    in
    Some
      (
        `CompletionList
          (L.CompletionList.create ~isIncomplete: false ~items ())
      )
