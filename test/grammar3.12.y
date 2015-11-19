%{
/* Grammar 3.12 from p47 of Appel (1998) */
%}

%token A C D

%%

z : D
  | x y z
  ;

y : C
  |
  ;

x : y
  | A
  ;

%%

