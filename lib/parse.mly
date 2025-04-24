%token <int> INT
%token EOF
%token SEMICOLON

%type <int> main
%start main
%%

main:
    | expr SEMICOLON { $1 }
;

expr:
    | INT { $1 }
;
