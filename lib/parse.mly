%{
  open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING

%token EOF
%token SEMICOLON

%type <Ast.program> main
%start main
%%

main:
    | constant { Prog $1 }
;

constant:
    | INT { Int $1 }
    | CHAR { Char $1 }
    | STRING { String $1 }
    | FLOAT { Float $1 }
;
