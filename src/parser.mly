%{
open Data
let productions : (production list) ref = ref []
let tokens : (string list) ref = ref []
let start = ref None
%}

%token <string> Ident
%token Colon Pipe Semi
%token Sep
%token Ptoken Pstart Ptype
%token EOF

%start main
%type <Data.element option * Data.production list> main

%%

main : section1 Sep section2 EOF  { (!start, !productions) }
     ;

section1 : Ptoken stringlist section1 { () }
         | Pstart Ident section1      { start := Some(Nonterminal($2)) }
         | Ptype Ident section1       { () }
         |                            { () }
         ;

stringlist : Ident stringlist    { tokens := $1 :: !tokens }
           |                     { () }
           ;

section2 : rule Semi section2    { () }
         |                       { () }
         ;

rule : Ident Colon identlist rule2 { let lhs = Nonterminal($1) in
                                     let ps = Prod(lhs, $3) :: $4 in
                                     let ps = List.map (fun y ->
                                                         let Prod(_,x) = y in
                                                           Prod(lhs, x))
                                                       ps in
                                       productions := !productions @ ps
                                   }
     ;

rule2 : Pipe identlist rule2     { let p = Prod(Nonterminal(""), $2) in
                                   p :: $3
                                 }
      |                          { [] }
      ;

identlist : Ident identlist      { let e = if List.mem $1 !tokens then
                                             Terminal($1)
                                           else
                                             Nonterminal($1)
                                   in e :: $2
                                 }
          |                      { [] }
          ;

