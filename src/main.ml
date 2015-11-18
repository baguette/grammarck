open Tokens
open Lexer

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Lexer.token lexbuf in
        print_string (token_repr result);
        print_string "\n"
    done
  with
  | Lexer.Eof ->
    ( print_string "Lexer finished successfully\n";
      exit 0
    )
  |Lexer.UnexpectedEof ->
    ( print_string "Unexpected end of file\n";
      exit 1
    )

