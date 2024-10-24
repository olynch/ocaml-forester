include Types

let parse lexbuf =
  match Grammar.datetime Lexer.token lexbuf with
  | datetime -> Some datetime
  | exception Grammar.Error ->
    None

let parse_string str =
  let lexbuf = Lexing.from_string str in
  parse lexbuf

let parse_string_exn str =
  match parse_string str with
  | None -> failwith "human datetime: parse error"
  | Some dt -> dt
