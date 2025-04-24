type constant = Int of int | Float of float | String of string | Char of char
[@@deriving show]

type operator = Add | Sub | Mult | Div [@@deriving show]

type expression =
  | BinaryExpr of operator * expression * expression
  | Const of constant
[@@deriving show]

type statement = Expression of expression | Constant of constant
[@@deriving show]

type type_spec = TypeInt [@@deriving show]

type declaration = FunctionDeclaration of type_spec * string * statement list
[@@deriving show]

type program = Prog of declaration [@@deriving show]
