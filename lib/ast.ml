(* C types. There are no structs, unions, typedefs or floating point yet.
   [Array (t, n)] is "array of n values of type t": for [int a[3][4]] it is
   [Array (Array (TypeInt, 4), 3)]. *)
type type_spec =
  | TypeVoid
  | TypeChar
  | TypeInt
  | TypeLong
  | Pointer of type_spec
  | Array of type_spec * int
[@@deriving show]

type constant = Int of int | Float of float | String of string | Char of char
[@@deriving show]

(* Binary operators. [LogAnd]/[LogOr] are && and ||, which (unlike the others)
   don't always evaluate their right operand. *)
type operator =
  | Add | Sub | Mult | Div | Mod
  | Lt | Gt | Le | Ge | NotEq | Eq
  | BitAnd | BitOr | BitXor | ShiftL | ShiftR
  | LogAnd | LogOr
[@@deriving show]

type unary_operator =
  | Negate  (* -x *)
  | LogNot  (* !x *)
  | BitNot  (* ~x *)
  | Deref   (* *p *)
  | AddrOf  (* &x *)
[@@deriving show]

(* Notes on what the parser does so later stages don't have to:
   - [a[i]] is rewritten to [*(a + i)], exactly as the C standard defines it.
   - [+x] is just [x].
   - Adjacent string literals ("a" "b") are already joined. *)
type expression =
  | BinaryExpr of operator * expression * expression
  | UnaryExpr of unary_operator * expression
  | Assign of expression * expression  (* lhs = rhs *)
  | CompoundAssign of operator * expression * expression  (* lhs += rhs, ... *)
  (* ++x, x--, ... [op] is Add or Sub; [prefix] says whether the expression's
     value is the new value (++x) or the old one (x++). *)
  | IncDec of { op : operator; prefix : bool; target : expression }
  | Conditional of expression * expression * expression  (* c ? a : b *)
  | Cast of type_spec * expression
  | SizeofType of type_spec
  | SizeofExpr of expression
  (* { a, b, c }: only valid as the initializer of an array. *)
  | InitList of expression list
  | Const of constant
  | Variable of string
  | FunctionCall of string * expression list  (* function name and arguments *)
[@@deriving show]

(* Where something was written, for error messages. [file] is the file after
   #include processing, so it can be a header. *)
type loc = { file : string; line : int } [@@deriving show]

type var_declaration = {
  type_spec : type_spec;
  ident : string;
  init : expression option;
  loc : loc;
}
[@@deriving show]

type statement =
  | Expression of expression
  | VarDeclaration of var_declaration
  | ReturnStatement of expression option
  | If of expression * statement * statement option
  | While of expression * statement
  | DoWhile of statement * expression
  (* [init] is a list because [for (int i = 0, j = 1; ...)] declares two
     variables. It's empty if the for has no initializer. *)
  | For of {
      init : statement list;
      cond : expression option;
      step : expression option;
      body : statement;
    }
  | Block of statement list  (* { ... }: opens a new variable scope *)
  | Break
  | Continue
  | Empty  (* a lone ; *)
  | Located of loc * statement  (* the parser wraps each statement with its position *)
[@@deriving show]

(* A parameter's name is "" when it is omitted, which C allows in prototypes:
   for example the prototype of puts. *)
type param = Param of { type_spec : type_spec; ident : string }
[@@deriving show]

(* [type_spec] is the return type. [body = None] makes this a prototype (a
   declaration without a definition). [variadic] is true for a trailing "...",
   as in [int printf(const char *fmt, ...)]. *)
type func_declaration = {
  type_spec : type_spec;
  ident : string;
  params : param list;
  variadic : bool;
  body : statement list option;
  loc : loc;
}
[@@deriving show]

type external_declaration =
  | FunctionDeclaration of func_declaration
  | GlobalVarDeclaration of var_declaration
[@@deriving show]

type program = Program of external_declaration list [@@deriving show]
