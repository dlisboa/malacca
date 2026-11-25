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

type operator = Add | Sub | Mult | Div | Lt | Gt | Le | Ge | NotEq | Eq
[@@deriving show]

type expression =
  | BinaryExpr of operator * expression * expression
  | Const of constant
  | Variable of string
  | FunctionCall of string * expression list  (* function name and arguments *)
[@@deriving show]

type var_declaration = {
  type_spec : type_spec;
  ident : string;
  init : expression option;
}
[@@deriving show]

type statement =
  | Expression of expression
  | VarDeclaration of var_declaration
  | ReturnStatement of expression option
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
}
[@@deriving show]

type external_declaration =
  | FunctionDeclaration of func_declaration
  | GlobalVarDeclaration of var_declaration
[@@deriving show]

type program = Program of external_declaration list [@@deriving show]
