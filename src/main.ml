open Data

let usage = "grammarck [task] <grammar.y>"

type task =
  | Usage
  | LL1

let mode = ref Usage
let filename = ref ""

let cli_LL1 () = mode := LL1
let arg s = filename := s

let cli = [
  ("--LL1", Arg.Unit(cli_LL1), "Check if grammar is LL(1)")
]



(* Main program *)
let _ =
  let _ = Arg.parse cli arg usage in
    ();

  match !mode with
  | Usage -> Arg.usage cli usage
  | LL1   ->
    try
      let lexbuf = Lexing.from_channel @@ open_in !filename in
      let productions = Parser.main Lexer.token lexbuf in
        print_string @@ production_list_repr productions
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

