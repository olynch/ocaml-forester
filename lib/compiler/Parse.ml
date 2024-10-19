(*
 * SPDX-FileCopyrightText: 2024 The Forester Project Contributors
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 *)

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

(*  NOTE:
    I am unsure if we should ever fail during lexing. I propose: We should
    introduce an `Error` syntax node so that the parse tree is capable of
    representing arbitrary erroneous input. We never want this during batch
    compilation, but always want this for the IDE. Depending on the context in
    which the parser is running, we can immediately fail when encountering
    such a node, or continue. This is what the `I.AboutToReduce` state is for.
  *)

type _error =
  | Unclosed of Range.position * string * Range.position * string
  | Expecting of Range.position * string

let position_of_error = function
  | Unclosed (pos, _, _, _)
  | Expecting (pos, _) ->
    pos

let get_parse_error env =
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

let get_range
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
    lexbuf ->
    (Code.t, Reporter.Message.t Asai.Diagnostic.t) Result.t
  = fun ?(stop_on_err = true) lexbuf ->

    (* a checkpoint is an intermediate or final state of the parser *)
    let initial_checkpoint = (Grammar.Incremental.main lexbuf.lex_curr_p) in

    (* The idea is simple: push opening delimiters onto the stack, pop them
       once they are closed. Upon encountering an error, the reported range
       includes the last unclosed delimiter.
     *)
    let delim_stack = Stack.create () in

    (* The recursive loop.*)
    let fail
        : _ I.checkpoint ->
        (Code.t, Reporter.Message.t Asai.Diagnostic.t) Result.t
      = fun checkpoint ->
        match checkpoint with
        (*
          Consider the source file

          ```
          \def\foo{

          \p{
            lorem ipsum dolor sit amet
          }
          ```

          Before this patch, the error reported when encountering this was
          undecipherable, since it just hit EOF and then reported unexpected ``.

          The diagnostic constructed below enlarges the range of the reported
          error to the last unclosed delimiter. This makes the error much more
          understandable

          ```
          ￭ ./trees/comment.tree
          1 | \def\foo{
            ^ This delimiter is never closed
          2 |
          3 | \p{
          4 |   lorem ipsum dolor sit amet
          5 | }
          6 |
            ^ Did you forget to close it?
          ```

          However, for implementing an LSP this is insufficient. When
          encountering an error, we produce no value. We need an
          error-resilient parser. We want the parse tree to look something like
          this:

          1 TREE
          2  FUN_SPEC
          3    NAME "foo"
          4    ARG_LIST []
          5    BODY ERROR
          6  PRIM PARAGRAPH
          7     TEXT "lorem ipsum dolor sit amet"

          The tricky bit here is that we only know that there is a parse error
          when encountering EOF. How can we derive the fact that the erroneous
          construct is the function definition, and that the error should be
          localised at the unclosed brace?

          We would need to inspect the parser state and recognize that we are
          currently parsing a function whose body never gets closed, produce a
          value like at line 5, and resume:
          https://gallium.inria.fr/%7Efpottier/menhir/manual.html#sec%3Aincremental%3Aupdating

          This illustrates the problem with having such strictly typed parse
          tree. Code.t was not able to represent such a failed parse. (I have
          sinced added an error node, so it is now technically possible for
          this specific usecase, but so far it is unused. Furhtermore, the code
          can't represent, for example, trie paths that failed to parse).

            Aside: Inspired by rust-analyzer and rowan, I am playing around
            with github.com/kentookura/orochi. In rust-analyzer, they use such
            an untyped API and layer a typed AST over it. I haven't gotten
            around to that part yet.

          Now the use case of the incremental API becomes apparent. It allows
          us to inspect the parser state and travel in time, look back, look
          forward, anything we want.

          Here are the action items:

          - Understand what kinds of syntax errors can occur when typing. These
          are the erroneous inputs we are most likely to encounter, as we want
          to make an LSP server. This is stuff like unclosed delimiters, etc.

          - Understand how we should best recover from erroneous input, for
          example:

          ```
          \ul{
            \li{
              asdf
            }
            {
          }
          ```

          Should report "unexpected {", rather than "unclosed {", and produce
          something like:

          1 TREE
          2   PRIM OL
          3     PRIM LI
          4       TEXT "asdf"
          5     ERROR "{"

          - Study the menhir API so that we know how to implement this kind of
          recovery.
           *)
        | I.HandlingError _ -> Reporter.fatalf Parse_error "TODO"
        | _ -> Reporter.fatalf Parse_error "TODO"
    in
    let rec run
        : _ I.checkpoint ->
        _ ->
        (Code.t, Reporter.Message.t Asai.Diagnostic.t) Result.t
      = fun checkpoint supplier ->
        match checkpoint with
        | I.InputNeeded _env ->
          (* In this phase we push and pop delimiters onto the stack.*)
          let token, start, end_ = supplier () in
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
        | I.HandlingError env ->
          if not stop_on_err then
            (* TODO: emit error node *)
            Error
              (
                Asai.Diagnostic.of_text
                  Error
                  Forester_core.Reporter.Message.Parse_error
                  (Asai.Diagnostic.text "")
              )
          else
            let err
              =
              get_parse_error env
            in
            let range_of_last_unclosed =
              Option.map snd @@ Stack.top_opt delim_stack
            in
            let loc =
              match Option.map Range.view range_of_last_unclosed with
              | Some (`Range (start_pos, _)) ->
                Range.make
                  (start_pos, Range.of_lex_position @@ Lexing.lexeme_start_p lexbuf)
              | Some (`End_of_file _) -> Range.of_lexbuf lexbuf
              | None -> Range.of_lexbuf lexbuf
            in
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
                Asai.Diagnostic.of_loctext
                  ~extra_remarks
                  Error
                  Forester_core.Reporter.Message.Parse_error
                  { loc = Some loc; value = err.value }
              )
        | I.Accepted code -> Ok code
        | I.Rejected ->
          assert false
    in
    let supplier = I.lexer_lexbuf_to_supplier lexer lexbuf in
    run initial_checkpoint supplier

let parse_lexbuf lexbuf =
  try
    parse lexbuf
  with
    | Grammar.Error ->
      Reporter.fatalf ~loc: (Range.of_lexbuf lexbuf) Parse_error "failed to parse"
    | exn -> raise exn

let parse_channel filename ch =
  let lexbuf = Lexing.from_channel ch in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_lexbuf lexbuf

let parse_string str =
  let lexbuf = Lexing.from_string str in
  parse_lexbuf lexbuf

let parse_file filename =
  let@ () = Reporter.tracef "when parsing file `%s`" filename in
  let ch = open_in filename in
  Fun.protect ~finally: (fun _ -> close_in ch) @@
    fun _ ->
      parse_channel filename ch
