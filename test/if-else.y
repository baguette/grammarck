
%token IF THEN ELSE

%%

exp : IF exp THEN exp
    | IF exp THEN exp ELSE exp
    ;

