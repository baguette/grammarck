
%token ADD SUB MUL DIV
%token LPAREN RPAREN
%token NUM
%token EOF

%%

start : exp EOF
      ;

exp : term exp2
    ;

exp2 : ADD term exp2
     | SUB term exp2
     |
     ;

term : factor term2
     ;

term2 : MUL factor term2
      | DIV factor term2
      |
      ;

factor : NUM
       | LPAREN exp RPAREN
       ;

