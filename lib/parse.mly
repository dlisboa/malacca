%{
  open Ast
%}

(* constants *)
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING

%token <string> IDENTIFIER

%token EOF
%token PLUS MINUS STAR SLASH
%token LBRACE RBRACE LPAREN RPAREN
%token SEMICOLON EQ
%token KEY_INT

%type <Ast.program> main
%start main
%%

main:
    | external_declaration_list EOF { Program $1}

external_declaration_list:
    | (* empty *) { [] }
    | external_declaration external_declaration_list { $1 :: $2 }

external_declaration:
    | function_declaration { $1 }
    | global_declaration { $1 }

global_declaration:
    | type_specifier declarator SEMICOLON { GlobalVarDeclaration { type_spec = $1; ident = $2; init = None } }
    | type_specifier declarator EQ expr SEMICOLON { GlobalVarDeclaration { type_spec = $1; ident = $2; init = Some $4 } }

function_declaration:
    | type_specifier declarator params_list compound_statement { FunctionDeclaration { type_spec = $1; ident = $2; params = []; body = $4 } }

type_specifier:
    | KEY_INT { TypeInt }

declarator:
    | IDENTIFIER { $1 }

params_list:
    | LPAREN RPAREN {}

compound_statement:
    | LBRACE statement_list RBRACE { $2 }

statement_list:
    | (* empty *) { [] }
    | statement statement_list { $1 :: $2 }

var_declaration:
    | type_specifier declarator EQ expr SEMICOLON { VarDeclaration { type_spec = $1; ident = $2; init = Some $4 } }

statement:
    | expr SEMICOLON { Expression $1 }
    | var_declaration { $1 }

expr:
    | binary_expr { $1 } 
    | constant { Const $1 }
    | IDENTIFIER { Variable $1 }

binary_expr:
    | expr PLUS expr { BinaryExpr (Add, $1, $3) }
    | expr MINUS expr { BinaryExpr (Sub, $1, $3) }
    | expr STAR expr { BinaryExpr (Mult, $1, $3) }
    | expr SLASH expr { BinaryExpr (Div, $1, $3) }

constant:
    | INT { Int $1 }
    | CHAR { Char $1 }
    | STRING { String $1 }
    | FLOAT { Float $1 }
