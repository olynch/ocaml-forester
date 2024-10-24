let digit = ['0'-'9']
let colon = ':'
let hyphen = '-'
let plus = '+'
let hyphen = '-'

rule token = parse
  | digit as d { Grammar.DIGIT d }
  | colon { Grammar.COLON }
  | hyphen { Grammar.HYPHEN }
  | plus { Grammar.PLUS }
  | 'T' { Grammar.T }
  | 'Z' { Grammar.Z }
  | eof { Grammar.EOF }

  | _ { failwith @@ Format.sprintf "Unexpected lexeme: %s" (Lexing.lexeme lexbuf) }
