{
  exception SyntaxError of string
  let drop_sigil c str = 1 |> List.nth @@ String.split_on_char c str
  let macro str = Parser.MACRO (drop_sigil '\\' str)
  let illegal str = raise @@ SyntaxError ("Lexer - Illegal character: [" ^ str ^ "].")
  
  let text str = Parser.TEXT str
  let dbg str = Format.printf "%s\n" str; flush stdout
  
  let verbatim = ref false
  
  let return_thunk lexbuf thunk = 
    match !verbatim with 
    | true -> text (Lexing.lexeme lexbuf)
    | false -> thunk ()

  let return lexbuf tok =
    return_thunk lexbuf @@ fun _ -> tok
    
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let int = '-'? digit+  
let macro = '\\' (alpha) (alpha|digit|'-')*
let addr = (alpha) (alpha|digit|'_'|'-')* 
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let text = [^ '#' '\\' '{' '}' '[' ']' '(' ')' '`' '\n']+
 
rule token =
  parse
  | "%" { comment lexbuf }
  | "#{" { return lexbuf @@ Parser.HASH_LBRACE }
  | "\\\\" { return lexbuf @@ Parser.MACRO {|\|} }
  | "\\," { return lexbuf @@ Parser.MACRO {|,|} }
  | "\\_" { return lexbuf @@ Parser.MACRO {|_|} }
  | "\\;" { return lexbuf @@ Parser.MACRO {|;|} }
  | "\\#" { return lexbuf @@ Parser.MACRO {|#|} }
  | "\\{" { return lexbuf @@ Parser.MACRO {|{|} }
  | "\\}" { return lexbuf @@ Parser.MACRO {|}|} }
  | "\\[" { return lexbuf @@ Parser.MACRO {|[|} }
  | "\\]" { return lexbuf @@ Parser.MACRO {|]|} }
  | "\\startverb" { verbatim := true; token lexbuf }
  | "\\stopverb" { verbatim := false; token lexbuf }
  | "\\ " { return lexbuf @@ Parser.MACRO {| |} }
  | "\\title" { return lexbuf @@ Parser.TITLE }
  | "\\taxon" { return lexbuf @@ Parser.TAXON }
  | "\\import" { return lexbuf @@ Parser.IMPORT }
  | "\\def" { return lexbuf @@ Parser.DEF }
  | "\\let" { return lexbuf @@ Parser.LET }
  | "\\tex" { return lexbuf @@ Parser.TEX }
  | "\\transclude" { return lexbuf @@ Parser.TRANSCLUDE }
  | "#" { return lexbuf @@ Parser.TEXT "#" }
  | macro { return lexbuf @@ macro (Lexing.lexeme lexbuf) }
  | '{' { return lexbuf @@ Parser.LBRACE }
  | '}' { return lexbuf @@ Parser.RBRACE }
  | '[' { return lexbuf @@ Parser.LSQUARE }
  | ']' { return lexbuf @@ Parser.RSQUARE }
  | '(' { return lexbuf @@ Parser.LPAREN }
  | ')' { return lexbuf @@ Parser.RPAREN }
  | text { text (Lexing.lexeme lexbuf) }
  | whitespace { return_thunk lexbuf @@ fun _ -> token lexbuf }
  | newline { Lexing.new_line lexbuf; return_thunk lexbuf @@ fun _ -> token lexbuf }
  | eof { Parser.EOF }
  | _ { illegal @@ Lexing.lexeme lexbuf }

and comment = 
  parse 
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }
