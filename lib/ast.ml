type type_spec = TypeInt [@@deriving show]

type constant = Int of int | Float of float | String of string | Char of char
[@@deriving show]

type operator = Add | Sub | Mult | Div [@@deriving show]

type expression =
  | BinaryExpr of operator * expression * expression
  | Const of constant
  | Variable of string
[@@deriving show]

type var_declaration = {
  type_spec : type_spec;
  ident : string;
  init : expression option;
}
[@@deriving show]

type statement = Expression of expression | VarDeclaration of var_declaration
[@@deriving show]

type func_declaration = {
  type_spec : type_spec;
  ident : string;
  params : (type_spec * string option) list;
  body : statement list;
}
[@@deriving show]

type external_declaration =
  | FunctionDeclaration of func_declaration
  | GlobalVarDeclaration of var_declaration
[@@deriving show]

type program = Program of external_declaration list [@@deriving show]
