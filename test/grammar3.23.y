/* Grammar 3.23 from Appel (1998) */

%token PLUS
%token X

%start e

%%

e : t PLUS e
  | t
  ;

t : X
  ;

