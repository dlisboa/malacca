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
(* punctuators *)
%token PLUS MINUS STAR SLASH
%token LBRACE RBRACE LPAREN RPAREN
%token SEMICOLON EQ COMMA
%token DOT QUESTION COLON BANG TILDE BIT_AND BIT_OR XOR LT LE GT GE ARROW EQ_EQ NOT_EQ 
%token LOG_AND LOG_OR SHIFTL SHIFTR PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ MOD_EQ BIT_AND_EQ
%token BIT_OR_EQ XOR_EQ SHIFTL_EQ SHIFTR_EQ MOD
(* keywords *)
%token KEY_AUTO  KEY_BREAK  KEY_CASE  KEY_CHAR  KEY_CONST  KEY_CONTINUE  KEY_DEFAULT  KEY_DO
%token KEY_DOUBLE  KEY_ELSE  KEY_ENUM  KEY_EXTERN  KEY_FLOAT  KEY_FOR  KEY_GOTO  KEY_IF
%token KEY_INLINE  KEY_INT  KEY_LONG  KEY_REGISTER  KEY_RESTRICT  KEY_RETURN  KEY_SHORT
%token KEY_SIGNED  KEY_SIZEOF  KEY_STATIC  KEY_STRUCT  KEY_SWITCH  KEY_TYPEDEF  KEY_UNION
%token KEY_UNSIGNED  KEY_VOID  KEY_VOLATILE  KEY_WHILE  KEY__ALIGNAS  KEY__ALIGNOF  KEY__ATOMIC
%token KEY__BOOL  KEY__COMPLEX  KEY__GENERIC  KEY__IMAGINARY  KEY__NORETURN  KEY__STATIC_ASSERT  KEY__THREAD_LOCAL

%nonassoc UNARY_MINUS ADDRESS DEREF

%left COMMA
%right EQ PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ MOD_EQ BIT_AND_EQ BIT_OR_EQ XOR_EQ SHIFT_LEFT_EQ SHIFT_RIGHT_EQ
%right QUESTION COLON
%left LOG_OR LOG_AND BIT_OR XOR BIT_AND
%left EQ_EQ NOT_EQ
%left LE LT GE GT
%left SHIFTL SHIFTR
%left PLUS MINUS
%left STAR SLASH MOD

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
    | type_specifier declarator LPAREN params_list RPAREN compound_statement { FunctionDeclaration { type_spec = $1; ident = $2; params = $4; body = $6 } }

type_specifier:
    | KEY_INT { TypeInt }

declarator:
    | IDENTIFIER { $1 }

params_list:
    | (* empty *) { [] }
    | type_specifier declarator { [Param { type_spec = $1; ident = $2 }] }
    | type_specifier declarator COMMA params_list { Param { type_spec = $1; ident = $2 } :: $4 }

compound_statement:
    | LBRACE statement_list RBRACE { $2 }

statement_list:
    | (* empty *) { [] }
    | statement statement_list { $1 :: $2 }

var_declaration:
    | type_specifier declarator EQ expr SEMICOLON { VarDeclaration { type_spec = $1; ident = $2; init = Some $4 } }
    | type_specifier declarator SEMICOLON { VarDeclaration { type_spec = $1; ident = $2; init = None } }

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
    | expr LT expr { BinaryExpr (Lt, $1, $3) }
    | expr LE expr { BinaryExpr (Le, $1, $3) }
    | expr GT expr { BinaryExpr (Gt, $1, $3) }
    | expr GE expr { BinaryExpr (Ge, $1, $3) }
    | expr EQ_EQ expr { BinaryExpr (Eq, $1, $3) }
    | expr NOT_EQ expr { BinaryExpr (NotEq, $1, $3) }

constant:
    | INT { Int $1 }
    | CHAR { Char $1 }
    | STRING { String $1 }
    | FLOAT { Float $1 }
