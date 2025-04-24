type constant = Int of int | Float of float | String of string | Char of char
[@@deriving show]

type program = Prog of constant [@@deriving show]
