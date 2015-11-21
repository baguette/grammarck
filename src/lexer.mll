{
  open Parser

  let linecount = ref 1
  let section = ref 1

  exception UnexpectedEof
  exception Error of int * string
}

let whitespace = ' ' | '\t'
let newline = '\n' | '\r' '\n'

let opdelim = ':' | '|' | '(' | ')' | '{' | '}' | ';'

let alpha = ['_' 'a'-'z' 'A'-'Z']
let alphanum = ['_' 'a'-'z' 'A'-'Z' '0'-'9']
let ident = alpha alphanum*

rule token = parse
  | whitespace { token lexbuf }
  | newline    { incr linecount;
                 token lexbuf
               }

  | "/*" { comment lexbuf }
  | "%{" { header  lexbuf }
  | '('  { paction lexbuf }
  | '{'  { caction lexbuf }
  | '<'  { qualifier lexbuf }

  | "%start" { Pstart }
  | "%type"  { Ptype  }
  | "%token" { Ptoken }
  | ident    { Ident(Lexing.lexeme lexbuf) }
  | "%%"     { incr section;
               if !section = 3 then
                 section3 lexbuf
               else
                 Sep
             }
  | ':'      { Colon }
  | '|'      { Pipe  }
  | ';'      { Semi  }
  | eof      { EOF   }
  | _        { raise (Error(!linecount,
                            "unexpected character: " ^ Lexing.lexeme lexbuf))
             }

and comment = parse
  | "*/" { token lexbuf }
  | newline
    { incr linecount;
      comment lexbuf
    }
  | eof { raise UnexpectedEof }
  | _ { comment lexbuf }

(* Ignore the header and the semantic actions since we
 * only care about the grammar
 *)
and header = parse
  | "%}" { token lexbuf }
  | newline
    { incr linecount;
      header lexbuf
    }
  | eof { raise UnexpectedEof }
  | _ { header lexbuf }

and paction = parse
  | ')' { token lexbuf }
  | newline
    { incr linecount;
      paction lexbuf
    }
  | eof { raise UnexpectedEof }
  | _ { paction lexbuf}

and caction = parse
  | '}' { token lexbuf }
  | newline
    { incr linecount;
      caction lexbuf
    }
  | eof { raise UnexpectedEof }
  | _ { caction lexbuf }

and qualifier = parse
  | '>' { token lexbuf }
  | newline
    { incr linecount;
      qualifier lexbuf
    }
  | eof { raise UnexpectedEof }
  | _ { qualifier lexbuf }

(* Ignore section 3 since it has real code we don't deal with *)
and section3 = parse
  | eof { EOF }
  | _ { section3 lexbuf }

