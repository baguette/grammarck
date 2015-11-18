%token ADD SUB NUM
%%
exp : exp ADD exp
    | exp SUB exp
    | NUM
    ;

