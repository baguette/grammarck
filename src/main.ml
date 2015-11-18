open Tokens
open Lexer

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Lexer.token lexbuf in
        Printf.printf "%s\n" (token_repr result);
    done
  with
  | Lexer.Eof ->
    ( print_string "Lexer finished successfully\n";
      exit 0
    )
  | Lexer.UnexpectedEof ->
    ( print_string "Unexpected end of file\n";
      exit 1
    )
  | Lexer.Error(n, s) ->
    ( Printf.printf "Lexical error line %d: %s\n" n s;
      exit 1
    )

