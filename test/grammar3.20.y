/* Grammar 3.20 from Appel (1998) */

%token LPAREN RPAREN
%token X
%token COMMA

%start s

%%

s : LPAREN l RPAREN
  | X
  ;

l : s
  | l COMMA s
  ;

%%

