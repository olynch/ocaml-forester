(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

module EP = Eio.Path

open Forester_prelude
open Forester_core
open Lexing

module I = Grammar.MenhirInterpreter

let buffer_lexer lexer =
  let buf = ref [] in
  let rec loop lexbuf =
    match !buf with
    | v :: vs ->
      buf := vs; v
    | [] ->
      match lexer lexbuf with
      | v :: vs -> buf := vs @ !buf; v
      | [] -> loop lexbuf
  in
  loop

let lexer =
  let@ lexbuf = buffer_lexer in
  match Stack.top @@ Lexer.mode_stack with
  | Main -> Lexer.token lexbuf
  | Ident_init -> Lexer.ident_init lexbuf
  | Ident_fragments -> Lexer.ident_fragments lexbuf
  | Verbatim (herald, buffer) -> Lexer.verbatim herald buffer lexbuf

let _get_parse_error env =
  let open Asai in
  match I.stack env with
  | lazy(Nil) -> Diagnostic.loctext "Did not expect text here."
  | lazy(Cons (I.Element (state, _, start, end_), _)) ->
    let loc = Range.of_lex_range (start, end_) in
    try
      let msg = (Grammar_messages.message (I.number state)) in
      Diagnostic.loctext ~loc msg
    with
      | Not_found -> Diagnostic.loctext ~loc "invalid syntax (no specific message for this eror)"

let _get_range
    : I.element option -> (position * position) option
  = fun el ->
    match el with
    | Some (I.Element (_, _, start_pos, end_pos)) ->
      Some (start_pos, end_pos)
    | None -> None

let closed_by c o =
  match (o, c) with
  | (Grammar.LSQUARE, Grammar.RSQUARE)
  | (Grammar.LPAREN, Grammar.RPAREN)
  | (Grammar.LBRACE, Grammar.RBRACE)
  | (Grammar.HASH_LBRACE, Grammar.RBRACE)
  | (Grammar.HASH_HASH_LBRACE, Grammar.RBRACE) ->
    true
  | _ -> false

let is_opening_delim = function
  | Grammar.LSQUARE
  | Grammar.LPAREN
  | Grammar.LBRACE
  | Grammar.HASH_LBRACE
  | Grammar.HASH_HASH_LBRACE ->
    true
  | _ -> false

let is_closing_delim = function
  | Grammar.RSQUARE
  | Grammar.RPAREN
  | Grammar.RBRACE ->
    true
  | _ -> false

let parse
    : ?stop_on_err: bool ->
    source: [`File of string | `String of Range.string_source] ->
    lexbuf ->
    (Code.t, Reporter.diagnostic) Result.t
  = fun ?(stop_on_err = true) ~source lexbuf ->
    let initial_checkpoint = (Grammar.Incremental.main lexbuf.lex_curr_p) in
    let delim_stack = Stack.create () in
    let rec run
        : _ I.checkpoint ->
        _ ->
        (Code.t, Reporter.diagnostic) Result.t
      = fun checkpoint supplier ->
        match checkpoint with
        | I.InputNeeded _env ->
          (* If the current token is an opening delimiter, save the
             token and its position on the stack.*)
          let token, _, _ = supplier () in
          let start_position = lexbuf.lex_start_p in
          let end_position = lexbuf.lex_curr_p in
          if is_opening_delim token then
            let range = Range.of_lex_range (start_position, end_position) in
            Stack.push (token, range) delim_stack;
          ;
          if is_closing_delim token then
            begin
              match Stack.top_opt delim_stack with
              | Some (open_delim, _) ->
                if (open_delim |> closed_by token) then
                  Stack.drop delim_stack
              | None -> ()
            end;
          let checkpoint = I.offer checkpoint (token, start_position, end_position) in
          run checkpoint supplier
        | I.Shifting((_, _, _): Code.t I.env * Code.t I.env * bool) ->
          let checkpoint = I.resume checkpoint ~strategy: `Simplified in
          run checkpoint supplier
        | I.AboutToReduce (_, _) ->
          let checkpoint = I.resume checkpoint ~strategy: `Simplified in
          run checkpoint supplier
        | I.HandlingError _ ->
          if not stop_on_err then
            (* TODO: Don't error out here *)
            Error
              (
                Asai.Diagnostic.of_text
                  ~loc: (Range.of_lexbuf ~source lexbuf)
                  Error
                  Reporter.Message.Parse_error
                  (Asai.Diagnostic.text "")
              )
          else
            let range_of_last_unclosed =
              Option.map snd @@ Stack.top_opt delim_stack
            in
            let loc = Range.of_lexbuf ~source lexbuf in
            let extra_remarks =
              if Option.is_some range_of_last_unclosed then
                [
                  Asai.Diagnostic.loctext
                    ?loc: range_of_last_unclosed
                    "This delimiter is never closed";
                ]
              else []
            in
            Error
              (
                Asai.Diagnostic.(
                  of_loctext
                    ~extra_remarks
                    Error
                    Forester_core.Reporter.Message.Parse_error
                    (loctext ~loc Format.(sprintf "syntax error, unexpected %S" (Lexing.lexeme lexbuf)))
                )
              )
        | I.Accepted code -> Ok code
        | I.Rejected ->
          assert false
    in
    let supplier = I.lexer_lexbuf_to_supplier lexer lexbuf in
    try
      run initial_checkpoint supplier
    with
      (* NOTE: This should be the only exception we ever need to catch here:
         The parser is driven manually, so we are responsible for creating the
         diagnostics. This means we should not use `fatalf`! This also means
         that we can safely use the returned diagnostic without worrying that
         there might be an unhandled Asai effect. *)
      | Lexer.SyntaxError lexeme ->
        let loc = Range.of_lexbuf ~source lexbuf in
        Error
          (
            Asai.Diagnostic.(
              of_loctext
                Error
                Reporter.Message.Parse_error
                (loctext ~loc Format.(sprintf "syntax error, unexpected %S" lexeme))
            )
          )

let parse_channel filename ch =
  let lexbuf = Lexing.from_channel ch in
  if filename = "" then assert false;
  Logs.debug (fun m -> m "parsing %s" filename);
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse ~source: (`File filename) lexbuf

let parse_document doc =
  let uri = Lsp.Text_document.documentUri doc in
  let path = Lsp.Uri.to_path uri in
  let text = Lsp.Text_document.text doc in
  let lexbuf = Lexing.from_string text in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = path };
  parse ~source: (`File path) lexbuf

let parse_file filename =
  let@ () = Reporter.tracef "when parsing file `%s`" filename in
  let ch = open_in filename in
  Fun.protect ~finally: (fun _ -> close_in ch) @@
    fun _ ->
      parse_channel filename ch
