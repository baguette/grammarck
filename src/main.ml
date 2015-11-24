open Data
open Funcs

let usage = "usage: " ^ Sys.argv.(0) ^ " [task] <grammar.y>"

type task =
  | Usage
  | LL1

let mode = ref Usage
let debug = ref false
let filename = ref ""

let cli_LL1 () = mode := LL1
let cli_dbg () = debug := true
let arg s = filename := s

let cli = [
  ("--LL1", Arg.Unit(cli_LL1), "Check if grammar is LL(1)");
  ("-g",    Arg.Unit(cli_dbg), "Display debug information")
]



(* Main program *)
let _ =
  let _ = Arg.parse cli arg usage in
    if !filename = "" then (
      Arg.usage cli usage;
      exit 1
    );

  match !mode with
  | Usage -> Arg.usage cli usage
  | _     ->
    try
      let lexbuf = Lexing.from_channel @@ open_in !filename in
      let productions = Parser.main Lexer.token lexbuf in
      let _ = if !debug then
                (print_string "PRODUCTIONS\n";
                 print_string @@ production_list_repr productions;
                 flush stdout)
              else
                ()
      in
      match !mode with
      | LL1 ->
        if !debug then (
          let first, follow, nullable = compute_first_follow productions in
          print_string "\nFIRST\n";
          print_tbl first;
          print_string "\nFOLLOW\n";
          print_tbl follow;
          print_string "\nPREDICTIVE PARSE TABLE\n";
          Ll1.print_parse_table productions;
          print_string "\nRESULTS\n";
        );
        Ll1.report_conflicts productions
      (* control should never reach this branch... *)
      | _ -> Arg.usage cli usage
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

