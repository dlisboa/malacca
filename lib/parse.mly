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

/* The "dangling else": in [if (a) if (b) x; else y;] the else belongs to the
   nearest if. Giving the else-less rule a lower precedence than KEY_ELSE makes
   the parser prefer shifting the else over reducing the shorter if. */
%nonassoc LOWER_THAN_ELSE
%nonassoc KEY_ELSE

/* Binary operator precedence, loosest first (yacc: later line = tighter).
   Only binary operators, assignment and ?: are resolved with this table; the
   unary and postfix levels are spelled out as separate rules below, following
   the layering of the C standard's grammar. */
%right EQ PLUS_EQ MINUS_EQ MULT_EQ DIV_EQ MOD_EQ BIT_AND_EQ BIT_OR_EQ XOR_EQ SHIFTL_EQ SHIFTR_EQ
%right QUESTION COLON
%left LOG_OR
%left LOG_AND
%left BIT_OR
%left XOR
%left BIT_AND
%left EQ_EQ NOT_EQ
%left LT LE GT GE
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

/* "const" may come before or after the type name ("const int", "int const");
   it is parsed and then ignored. */
base_type:
    | plain_type { $1 }
    | KEY_CONST plain_type { $2 }
    | plain_type KEY_CONST { $1 }

plain_type:
    | KEY_VOID { TypeVoid }
    | KEY_CHAR { TypeChar }
    | KEY_INT { TypeInt }
    | KEY_LONG { TypeLong }
    | KEY_LONG KEY_INT { TypeLong }

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

/* The items of a { ... } block: statements and declarations, in any order. */
statement_list:
    | (* empty *) { [] }
    | statement statement_list { $1 :: $2 }
    | declaration SEMICOLON statement_list
        { List.map (fun d -> VarDeclaration d) $1 @ $3 }

statement:
    | expr SEMICOLON { Expression $1 }
    | SEMICOLON { Empty }
    | compound_statement { Block $1 }
    | KEY_RETURN expr SEMICOLON { ReturnStatement (Some $2) }
    | KEY_RETURN SEMICOLON { ReturnStatement None }
    | KEY_IF LPAREN expr RPAREN statement %prec LOWER_THAN_ELSE { If ($3, $5, None) }
    | KEY_IF LPAREN expr RPAREN statement KEY_ELSE statement { If ($3, $5, Some $7) }
    | KEY_WHILE LPAREN expr RPAREN statement { While ($3, $5) }
    | KEY_DO statement KEY_WHILE LPAREN expr RPAREN SEMICOLON { DoWhile ($2, $5) }
    | KEY_FOR LPAREN for_init optional_expr SEMICOLON optional_expr RPAREN statement
        { For { init = $3; cond = $4; step = $6; body = $8 } }
    | KEY_BREAK SEMICOLON { Break }
    | KEY_CONTINUE SEMICOLON { Continue }

/* The first part of a for(;;). It includes its own semicolon, because a
   declaration and an expression statement each already end with one. */
for_init:
    | SEMICOLON { [] }
    | expr SEMICOLON { [Expression $1] }
    | declaration SEMICOLON { List.map (fun d -> VarDeclaration d) $1 }

optional_expr:
    | (* empty *) { None }
    | expr { Some $1 }

/* ---- Expressions ---- */

expr:
    | cast_expr { $1 }
    | expr PLUS expr { BinaryExpr (Add, $1, $3) }
    | expr MINUS expr { BinaryExpr (Sub, $1, $3) }
    | expr STAR expr { BinaryExpr (Mult, $1, $3) }
    | expr SLASH expr { BinaryExpr (Div, $1, $3) }
    | expr MOD expr { BinaryExpr (Mod, $1, $3) }
    | expr LT expr { BinaryExpr (Lt, $1, $3) }
    | expr LE expr { BinaryExpr (Le, $1, $3) }
    | expr GT expr { BinaryExpr (Gt, $1, $3) }
    | expr GE expr { BinaryExpr (Ge, $1, $3) }
    | expr EQ_EQ expr { BinaryExpr (Eq, $1, $3) }
    | expr NOT_EQ expr { BinaryExpr (NotEq, $1, $3) }
    | expr BIT_AND expr { BinaryExpr (BitAnd, $1, $3) }
    | expr BIT_OR expr { BinaryExpr (BitOr, $1, $3) }
    | expr XOR expr { BinaryExpr (BitXor, $1, $3) }
    | expr SHIFTL expr { BinaryExpr (ShiftL, $1, $3) }
    | expr SHIFTR expr { BinaryExpr (ShiftR, $1, $3) }
    | expr LOG_AND expr { BinaryExpr (LogAnd, $1, $3) }
    | expr LOG_OR expr { BinaryExpr (LogOr, $1, $3) }
    | expr QUESTION expr COLON expr { Conditional ($1, $3, $5) }
    /* The left side of an assignment must be a unary_expr (a variable, *p,
       a[i]...), which is why it isn't a plain expr. */
    | unary_expr EQ expr { Assign ($1, $3) }
    | unary_expr PLUS_EQ expr { CompoundAssign (Add, $1, $3) }
    | unary_expr MINUS_EQ expr { CompoundAssign (Sub, $1, $3) }
    | unary_expr MULT_EQ expr { CompoundAssign (Mult, $1, $3) }
    | unary_expr DIV_EQ expr { CompoundAssign (Div, $1, $3) }
    | unary_expr MOD_EQ expr { CompoundAssign (Mod, $1, $3) }
    | unary_expr BIT_AND_EQ expr { CompoundAssign (BitAnd, $1, $3) }
    | unary_expr BIT_OR_EQ expr { CompoundAssign (BitOr, $1, $3) }
    | unary_expr XOR_EQ expr { CompoundAssign (BitXor, $1, $3) }
    | unary_expr SHIFTL_EQ expr { CompoundAssign (ShiftL, $1, $3) }
    | unary_expr SHIFTR_EQ expr { CompoundAssign (ShiftR, $1, $3) }

/* "(type) expr". Casts sit between unary and binary operators in C: the
   operand of -, !, *, & is a cast_expr, which is why "-(char)x" works. */
cast_expr:
    | unary_expr { $1 }
    | LPAREN type_name RPAREN cast_expr { Cast ($2, $4) }

unary_expr:
    | postfix_expr { $1 }
    | MINUS cast_expr { UnaryExpr (Negate, $2) }
    | PLUS cast_expr { $2 }
    | BANG cast_expr { UnaryExpr (LogNot, $2) }
    | TILDE cast_expr { UnaryExpr (BitNot, $2) }
    | STAR cast_expr { UnaryExpr (Deref, $2) }
    | BIT_AND cast_expr { UnaryExpr (AddrOf, $2) }
    | INC unary_expr { IncDec { op = Add; prefix = true; target = $2 } }
    | DEC unary_expr { IncDec { op = Sub; prefix = true; target = $2 } }
    | KEY_SIZEOF unary_expr { SizeofExpr $2 }
    | KEY_SIZEOF LPAREN type_name RPAREN { SizeofType $3 }

postfix_expr:
    | primary_expr { $1 }
    | IDENTIFIER LPAREN argument_list RPAREN { FunctionCall ($1, $3) }
    | postfix_expr LBRACKET expr RBRACKET
        { UnaryExpr (Deref, BinaryExpr (Add, $1, $3)) }  /* a[i] == *(a + i) */
    | postfix_expr INC { IncDec { op = Add; prefix = false; target = $1 } }
    | postfix_expr DEC { IncDec { op = Sub; prefix = false; target = $1 } }

primary_expr:
    | INT { Const (Int $1) }
    | CHAR { Const (Char $1) }
    | FLOAT { Const (Float $1) }
    | string_literal { Const (String $1) }
    | IDENTIFIER { Variable $1 }
    | LPAREN expr RPAREN { $2 }

/* "Hello, " "world" is one string: adjacent literals are concatenated. */
string_literal:
    | STRING { $1 }
    | STRING string_literal { $1 ^ $2 }

argument_list:
    | (* empty *) { [] }
    | argument_list_nonempty { $1 }

argument_list_nonempty:
    | expr { [$1] }
    | expr COMMA argument_list_nonempty { $1 :: $3 }
