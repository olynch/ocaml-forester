open Forester_core
open Forester_compiler

module L = Lsp.Types

module State = Base.State

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
    ];;
end

let legend =
  L.SemanticTokensLegend.create
    ~tokenTypes: Token_type.token_types
    ~tokenModifiers: Token_modifiers_set.list

type token = {
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

let tokens =
  List.filter_map
    (
      fun
          Range.{ loc; value }
        ->
        let (L.Range.{ start; end_ }) = LspShims.Loc.lsp_range_of_range loc in
        (* Multiline tokens not supported*)
        if start.line <> end_.line then
          None
        else
          let line = start.line in
          let start_char = start.character in
          let length = end_.character - start.character in
          let token_type =
            match value with
            | Code.Text _
            | Code.Math (_, _)
            | Code.Ident _
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
            | Code.Put (_, _)
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
              1
          in
          Some { line; start_char; length; token_type; token_modifiers = 0 }
    )

let process_line
    : int option -> token list -> int * delta_token list
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
              ({ start_char; length; token_type; token_modifiers; line } as current_token)
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

let delta_tokens tokens =
  tokens
  |> List.fold_left
    (
      fun (last_line, acc) tokens_on_line ->
        let line, delta_tokens = process_line last_line tokens_on_line in
        Some line, delta_tokens :: acc
    )
    (None, [])
  |> snd
  |> List.rev
  |> List.concat
  |> List.concat_map encode_deltas
  |> List.rev
  |> Array.of_list

let semantic_tokens code =
  Some
    {
      L.SemanticTokens.resultId = None;
      data = (
        code
        |> tokens
        |> group (fun s t -> t.line = s.line)
        |> delta_tokens
      );
    }

let get_doc_and_tokenize (textDocument : L.TextDocumentIdentifier.t) =
  let server = State.get () in
  let doc = Hashtbl.find_opt server.codes { uri = textDocument.uri } in
  match doc with
  | None -> None
  | Some tree ->
    let res = semantic_tokens tree.code in
    Eio.traceln "";
    res

let on_full_request
    : L.SemanticTokensParams.t ->
    L.SemanticTokens.t option
  = fun
      { textDocument; _ }
    ->
    (get_doc_and_tokenize textDocument)

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
      (fun tokens -> (`SemanticTokens tokens))
      (get_doc_and_tokenize textDocument)
