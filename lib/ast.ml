type constant = Int of int | Float of float | String of string | Char of char
[@@deriving show]

type operator = Add | Sub | Mult | Div [@@deriving show]

type expression =
  | BinaryExpr of operator * expression * expression
  | Const of constant
[@@deriving show]

type statement = Expression of expression | Constant of constant
[@@deriving show]

type program = Prog of statement list [@@deriving show]
