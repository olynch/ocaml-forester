(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors AND The RedPRL Development Team
 *
 * SPDX-License-Identifier: GPL-3.0-or-later OR Apache-2.0 WITH LLVM-exception
 *
 *)

open Forester_core
open Forester_compiler
open Forester_forest

module L = Lsp.Types

let print_array =
  Format.(
    pp_print_array
      ~pp_sep: (fun out () -> fprintf out "; ")
      pp_print_int
  )

module Token_type = struct
  type t = L.SemanticTokenTypes.t
  let legend : L.SemanticTokenTypes.t list =
    [
      Namespace;
      Type;
      Class;
      Enum;
      Interface;
      Struct;
      TypeParameter;
      Parameter;
      Variable;
      Property;
      EnumMember;
      Event;
      Function;
      Method;
      Macro;
      Keyword;
      Modifier;
      Comment;
      String;
      Number;
      Regexp;
      Operator;
      Decorator
    ]
  let of_builtin t = t

  let token_types =
    List.map
      (
        fun s ->
          match L.SemanticTokenTypes.yojson_of_t s with
          | `String s -> s
          | _ -> assert false
      )
      legend

  let to_int =
    let module Table = MoreLabels.Hashtbl in
    let table =
      lazy(
        let t = Table.create (List.length legend) in
        List.iteri (fun data key -> Table.add t ~key ~data) legend;
        t
      )
    in
    fun t -> Table.find (Lazy.force table) t

  let to_legend t =
    match L.SemanticTokenTypes.yojson_of_t t with
    | `String s -> s
    | _ -> assert false
end

module Token_modifiers_set = struct
  let list =
    [
      "declaration";
      "definition";
      "readonly";
      "static";
      "deprecated";
      "abstract";
      "async";
      "modification";
      "documentation";
      "defaultLibrary"
    ]
  ;;
end

let legend =
  L.SemanticTokensLegend.create
    ~tokenTypes: Token_type.token_types
    ~tokenModifiers: Token_modifiers_set.list

type token = {
  (* node: string; *)
  line: int;
  start_char: int;
  length: int;
  token_type: int;
  token_modifiers: int;
}
[@@deriving show]

type delta_token = {
  delta_line: int;
  delta_start_char: int;
  length: int;
  token_type: int;
  token_modifiers: int;
}
[@@deriving show]

let encode : token -> int list = fun
      { line; start_char; length; token_type; token_modifiers; _ }
    ->
    [line; start_char; length; token_type; token_modifiers]

let encode_deltas : delta_token -> int list = fun
      { delta_line; delta_start_char; length; token_type; token_modifiers }
    ->
    [delta_line; delta_start_char; length; token_type; token_modifiers]

let group f l =
  let rec grouping acc = function
    | [] -> acc
    | hd :: tl ->
      let l1, l2 = List.partition (f hd) tl in
      grouping ((hd :: l1) :: acc) l2
  in
  grouping [] l

let node_to_tokens : Code.node Range.located -> _ list = fun _ -> []

let tokenize_path ~(start : L.Position.t) path =
  let offset = ref (start.character) in
  Eio.traceln "path has %i segments" (List.length path);
  path
  |> List.map
    (
      fun segment ->
        let length = String.length segment in
        let start_char = !offset in
        offset := !offset + length + 1;
        let token_type = 1 in
        let line = start.line in
        [
          {
            line;
            start_char;
            length;
            token_type;
            token_modifiers = 0;
            (* node = segment *)
          }
        ]
    )
  |> List.concat

let shift offset =
  List.map
    (
      fun token ->
        {
          token with
          start_char = token.start_char + offset
        }
    )

let builtin ~(start : L.Position.t) str tks =
  let offset = String.length str in
  {
    (* node = str; *)
    line = start.line;
    start_char = start.character;
    length = offset;
    token_type = 5;
    token_modifiers = 0
  } :: shift offset tks

let tokens
    : Code.t -> token list
  = fun nodes ->
    nodes
    |> List.concat_map
      (
        fun
            Range.{ loc; value }
          ->
          let (L.Range.{ start; end_ }) = Lsp_shims.Loc.lsp_range_of_range loc in
          (* Multiline tokens not supported*)
          if start.line <> end_.line then
            []
          else
            match value with
            | Code.Ident path ->
              tokenize_path ~start path
            | Code.Text _ -> []
            | Code.Put (_path, _t) -> []
            (* -> *)
            (*     builtin *)
            (*       ~start *)
            (*       "put" @@ *)
            (*     tokenize_path ~start path @ tokens t *)
            | Code.Math (_, _)
            | Code.Verbatim _
            | Code.Import (_, _)
            | Code.Let (_, _, _)
            | Code.Def (_, _, _)
            | Code.Group (_, _)
            | Code.Hash_ident _
            | Code.Xml_ident (_, _)
            | Code.Subtree (_, _)
            | Code.Open _
            | Code.Scope _
            | Code.Default (_, _)
            | Code.Get _
            | Code.Fun (_, _)
            | Code.Object _
            | Code.Patch _
            | Code.Call (_, _)
            | Code.Decl_xmlns (_, _)
            | Code.Alloc _
            | Code.Dx_sequent (_, _)
            | Code.Dx_query (_, _, _)
            | Code.Dx_prop (_, _)
            | Code.Dx_var _
            | Code.Dx_const_content _
            | Code.Dx_const_iri _
            | Code.Error _
            | Code.Comment _
            | Code.Namespace (_, _) ->
              []
      )

let process_line_delta
    : int option ->
    token list ->
    int * delta_token list
  = fun
      index_of_last_line
      tokens
    ->
    let line = (List.hd tokens).line in
    let deltas =
      List.fold_left
        (
          fun
              (last_token, acc)
              ({
                start_char;
                length;
                token_type;
                token_modifiers;
                line;
                _;
              } as current_token)
            ->
            match last_token with
            | None ->
              let delta_line = match index_of_last_line with Some i -> i - line | None -> line in
              let delta_start_char = start_char in
              let t = { delta_line; delta_start_char; length; token_type; token_modifiers } in
              (Some current_token, t :: acc)
            | Some last_token ->
              (*If there is a previous token, we know we are still on the same line*)
              let delta_line = current_token.line - last_token.line in
              let delta_start_char = if delta_line > 0 then current_token.start_char else current_token.start_char - last_token.start_char in
              let delta = { delta_line; delta_start_char; length = current_token.length; token_type = current_token.token_type; token_modifiers; } in
              (Some current_token, delta :: acc)
        )
        (None, [])
        tokens
    in
    (line, (snd deltas |> List.rev))

let delta_tokens
    : token list list -> int array
  = fun
      tokens
    ->
    tokens
    |> List.fold_left
      (
        fun (last_line, acc) tokens_on_line ->
          let line, delta_tokens = process_line_delta last_line tokens_on_line in
          Some line, delta_tokens :: acc
      )
      (None, [])
    |> snd
    |> List.rev
    |> List.concat
    |> List.concat_map encode_deltas
    |> List.rev
    |> Array.of_list

let semantic_tokens_delta
    : Code.node Range.located list -> L.SemanticTokensDelta.t
  = fun _code ->
    {
      L.SemanticTokensDelta.resultId = None;
      edits = [];
    }

let tokenize_document
    : L.TextDocumentIdentifier.t ->
    L.SemanticTokens.t option
  = fun { uri } ->
    let Lsp_state.{ forest; _ } = Lsp_state.get () in
    match Iri_resolver.(resolve (Uri uri) To_code forest) with
    | Some { code; _ } ->
      let tokens = tokens code in
      Format.(
        Eio.traceln
          "%a"
          (
            pp_print_list
              ~pp_sep: (fun out () -> fprintf out "; ")
              pp_token
          )
          tokens
      );
      let encoded = List.concat_map encode tokens in
      let data = Array.of_list @@ encoded in
      Some { data; resultId = None }
    | None ->
      None

let tokenize_document_delta
    : L.TextDocumentIdentifier.t -> L.SemanticTokensDelta.t option
  = fun
      textDocument
    ->
    let Lsp_state.{ forest; _ } = Lsp_state.get () in
    match Iri_resolver.(resolve (Uri textDocument.uri) To_code forest) with
    | None -> None
    | Some tree ->
      Some (semantic_tokens_delta tree.code)

let on_full_request
    : L.SemanticTokensParams.t ->
    L.SemanticTokens.t option
  = fun
      { textDocument; _ }
    ->
    (tokenize_document textDocument)

let on_delta_request
    : L.SemanticTokensDeltaParams.t ->
    [`SemanticTokens of L.SemanticTokens.t
    | `SemanticTokensDelta of L.SemanticTokensDelta.t] option
  = fun
      {
        textDocument;
        _;
      }
    ->
    Option.map
      (fun tokens -> (`SemanticTokensDelta tokens))
      (tokenize_document_delta textDocument)
