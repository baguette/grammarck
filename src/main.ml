open Data

let (<|) f x = f x

(* Main program *)
let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let productions = Parser.main Lexer.token lexbuf in
      print_string <| production_list_repr productions;
  with
  | Lexer.UnexpectedEof ->
    ( print_string "Unexpected end of file\n";
      exit 1
    )
  | Lexer.Error(n, s) ->
    ( Printf.printf "Lexical error line %d: %s\n" n s;
      exit 1
    )
  | Parsing.Parse_error ->
    ( Printf.printf "Syntax error\n";
      exit 1
    )

