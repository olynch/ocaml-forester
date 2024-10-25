{
  open Forester_prelude

  type mode = Main | Ident_init | Ident_fragments | Verbatim of string * Buffer.t
  let mode_stack = Stack.of_seq @@ List.to_seq [Main]

  let push_mode mode = Stack.push mode mode_stack
  let drop_mode () = Stack.drop mode_stack
  let set_mode mode = drop_mode(); push_mode mode
  let push_verbatim_mode herald = push_mode @@ Verbatim (herald, Buffer.create 2000)

  let raise_err lexbuf =
    let loc = Asai.Range.of_lexbuf lexbuf in
    Forester_core.Reporter.fatalf ~loc Forester_core.Reporter.Message.Parse_error "unrecognized token `%s`" @@
    String.escaped @@ Lexing.lexeme lexbuf
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let special_name = ['%' '\\' ',' '"' '`' '_' ';' '#' '{' '}' '[' ']' ' ']
let simple_name = (alpha | digit | '-')*

let xml_base_ident = (alpha) (alpha | digit | '-' | '_')*
let xml_qname = (xml_base_ident ':' xml_base_ident) | xml_base_ident
let wschar = [' ' '\t']
let newline = ['\r' '\n'] | "\r\n"
let newline_followed_by_ws = (newline) (wschar)*
let text = [^' ' '%' '#' '\\' '{' '}' '[' ']' '(' ')' '\r' '\n']+
let verbatim_herald = [^' ' '\t' '\r' '\n' '|']+

rule token = parse
  | "\\" { push_mode Ident_init; [] }
  | "%" { comment lexbuf }
  | "##{" { [Grammar.HASH_HASH_LBRACE] }
  | "#{" { [Grammar.HASH_LBRACE] }
  | "'" { [Grammar.TICK] }
  | '@' { [Grammar.AT_SIGN] }
  | "-:" { [Grammar.DX_ENTAILED] }
  | "#" { [Grammar.HASH] }
  | '#' (simple_name as name) { [Grammar.HASH_IDENT name] }
  | '?' (simple_name as name) { [Grammar.DX_VAR name] }
  | '{' { [Grammar.LBRACE] }
  | '}' { [Grammar.RBRACE] }
  | '[' { [Grammar.LSQUARE] }
  | ']' { [Grammar.RSQUARE] }
  | '(' { [Grammar.LPAREN] }
  | ')' { [Grammar.RPAREN] }
  | text as str { [Grammar.TEXT str] }
  | wschar+ as str { [Grammar.WHITESPACE str] }
  | newline as str { Lexing.new_line lexbuf; [Grammar.WHITESPACE str] }
  | eof { [Grammar.EOF] }
  | _ { raise_err lexbuf }

and ident_init = parse
  | "verb" (verbatim_herald as herald) '|' { drop_mode (); push_verbatim_mode herald; [] }
  | "startverb" { drop_mode (); push_verbatim_mode "\\stopverb"; [] }
  | "scope" { drop_mode (); [Grammar.SCOPE] }
  | "put" { drop_mode (); [Grammar.PUT] }
  | "put?" { drop_mode (); [Grammar.DEFAULT] }
  | "get" { drop_mode (); [Grammar.GET] }
  | "import" { drop_mode (); [Grammar.IMPORT] }
  | "export" { drop_mode (); [Grammar.EXPORT] }
  | "namespace" { drop_mode (); [Grammar.NAMESPACE] }
  | "open" { drop_mode (); [Grammar.OPEN] }
  | "def" { drop_mode (); [Grammar.DEF] }
  | "alloc" { drop_mode (); [Grammar.ALLOC] }
  | "let" { drop_mode (); [Grammar.LET] }
  | "fun" { drop_mode (); [Grammar.FUN] }
  | "subtree" { drop_mode (); [Grammar.SUBTREE] }
  | "object" { drop_mode (); [Grammar.OBJECT] }
  | "patch" { drop_mode (); [Grammar.PATCH] }
  | "call" { drop_mode (); [Grammar.CALL] }
  | "datalog" { drop_mode (); [Grammar.DATALOG] }
  | "<" (xml_base_ident as prefix) ':' (xml_base_ident as uname) ">" { drop_mode (); [XML_ELT_IDENT (Some prefix, uname)] }
  | "<" (xml_base_ident as uname) ">" { drop_mode (); [XML_ELT_IDENT (None, uname)] }
  | "xmlns:" (xml_base_ident as str) { drop_mode (); [DECL_XMLNS str] }
  | (simple_name as s) "/" { set_mode Ident_fragments; [Grammar.IDENT s; Grammar.SLASH] }
  | simple_name as s { drop_mode (); [Grammar.IDENT s] }
  | special_name as c { drop_mode (); [Grammar.IDENT (String.make 1 c)] }
  | _ { raise_err lexbuf }

and ident_fragments = parse
  | (simple_name as s) "/" { [Grammar.IDENT s; Grammar.SLASH] }
  | simple_name as s { drop_mode (); [Grammar.IDENT s] }
  | _ { raise_err lexbuf }

and comment = parse
  | newline_followed_by_ws { Lexing.new_line lexbuf; token lexbuf }
  | eof { [Grammar.EOF] }
  | _ { comment lexbuf }

and verbatim herald buffer = parse
  | newline as c
    {
      Lexing.new_line lexbuf;
      Buffer.add_string buffer c;
      []
    }
  | _ as c
    {
      Buffer.add_char buffer c;
      let buff_len = Buffer.length buffer in
      let herald_len = String.length herald in
      let offset = buff_len - herald_len in
      if offset >= 0 && Buffer.sub buffer offset herald_len = herald then
        let text =
          String_util.trim_trailing_whitespace @@
          String_util.trim_newlines @@
          Buffer.sub buffer 0 offset
        in
        drop_mode ();
        [Grammar.VERBATIM text]
      else
        []
    }
