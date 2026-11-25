%{
  open Ast

  (* Build [int a[2][3]]'s type from its base type and dimensions [2; 3]:
     Array (Array (int, 3), 2). *)
  let array_of dims base =
    List.fold_right (fun n t -> Array (t, n)) dims base

  (* "f(void)" means "no parameters": a single unnamed void parameter. *)
  let drop_void_param = function
    | [ Param { type_spec = TypeVoid; ident = "" } ] -> []
    | params -> params
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
%token SEMICOLON EQ COMMA LBRACKET RBRACKET INC DEC ELLIPSIS
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
    | external_declaration_list EOF { Program (List.concat $1) }

external_declaration_list:
    | (* empty *) { [] }
    | external_declaration external_declaration_list { $1 :: $2 }

/* A top-level item is a function (definition or prototype) or a list of global
   variables. Both start with "base_type stars IDENTIFIER", so the parser only
   knows which one it has when it sees "(" (function) or anything else. */
external_declaration:
    | base_type stars IDENTIFIER LPAREN params RPAREN compound_statement
        { let params, variadic = $5 in
          [FunctionDeclaration { type_spec = $2 $1; ident = $3; params; variadic; body = Some $7 }] }
    | base_type stars IDENTIFIER LPAREN params RPAREN SEMICOLON
        { let params, variadic = $5 in
          [FunctionDeclaration { type_spec = $2 $1; ident = $3; params; variadic; body = None }] }
    | declaration SEMICOLON { List.map (fun d -> GlobalVarDeclaration d) $1 }

/* ---- Types ----
   C declarations read "base type, then for each name its own stars/arrays":
   in [int *a, b;] a is a pointer but b is a plain int. So the base type and
   the stars are separate nonterminals. [stars] returns a *function* that wraps
   a type in the right number of Pointer constructors. */

base_type:
    | KEY_VOID { TypeVoid }
    | KEY_CHAR { TypeChar }
    | KEY_INT { TypeInt }
    | KEY_LONG { TypeLong }
    | KEY_LONG KEY_INT { TypeLong }
    | KEY_CONST base_type { $2 }    /* const is parsed and then ignored */
    | base_type KEY_CONST { $1 }

stars:
    | (* empty *) { fun t -> t }
    | STAR qualifiers stars { fun t -> $3 (Pointer t) }

qualifiers:
    | (* empty *) { () }
    | KEY_CONST qualifiers { () }

/* A type on its own, as in a cast or sizeof: [(char *)] */
type_name:
    | base_type stars { $2 $1 }

/* ---- Declarations (shared by globals and locals) ---- */

declaration:
    | base_type init_declarator_list { List.map (fun make -> make $1) $2 }

init_declarator_list:
    | init_declarator { [$1] }
    | init_declarator COMMA init_declarator_list { $1 :: $3 }

init_declarator:
    | stars IDENTIFIER array_suffix
        { fun base -> { type_spec = array_of $3 ($1 base); ident = $2; init = None } }
    | stars IDENTIFIER array_suffix EQ expr
        { fun base -> { type_spec = array_of $3 ($1 base); ident = $2; init = Some $5 } }

/* "[2][3]" -> [2; 3] */
array_suffix:
    | (* empty *) { [] }
    | LBRACKET INT RBRACKET array_suffix { $2 :: $4 }

/* ---- Parameters ---- */

params:
    | (* empty *) { ([], false) }
    | param_list { (drop_void_param $1, false) }
    | param_list COMMA ELLIPSIS { ($1, true) }

/* Left-recursive on purpose: after "param_list ," the parser must still be
   able to choose between another param and the "..." that ends a variadic list. */
param_list:
    | param { [$1] }
    | param_list COMMA param { $1 @ [$3] }

param:
    | base_type stars param_name param_array
        { Param { type_spec = $4 ($2 $1); ident = $3 } }

param_name:
    | (* empty *) { "" }
    | IDENTIFIER { $1 }

/* Arrays decay to pointers when passed to functions: [char *argv[]] is
   [char **argv], and [int a[10]] is [int *a]. */
param_array:
    | (* empty *) { fun t -> t }
    | LBRACKET RBRACKET { fun t -> Pointer t }
    | LBRACKET INT RBRACKET { fun t -> Pointer t }

compound_statement:
    | LBRACE statement_list RBRACE { $2 }

statement_list:
    | (* empty *) { [] }
    | statement statement_list { $1 :: $2 }
    | declaration SEMICOLON statement_list
        { List.map (fun d -> VarDeclaration d) $1 @ $3 }

statement:
    | expr SEMICOLON { Expression $1 }
    | KEY_RETURN expr SEMICOLON { ReturnStatement (Some $2) }
    | KEY_RETURN SEMICOLON { ReturnStatement None }

expr:
    | binary_expr { $1 }
    | constant { Const $1 }
    | IDENTIFIER { Variable $1 }
    | IDENTIFIER LPAREN argument_list RPAREN { FunctionCall ($1, $3) }

argument_list:
    | (* empty *) { [] }
    | expr { [$1] }
    | expr COMMA argument_list { $1 :: $3 }

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
