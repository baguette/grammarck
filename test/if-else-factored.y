
%token IF THEN ELSE

%%

exp : IF exp THEN exp exp2
    ;

exp2 : ELSE exp
     |
     ;

