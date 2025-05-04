open Ast

(*.globl	_main                           ; -- Begin function main*)
(*.p2align	2*)
(*_main:                                  ; @main*)
(*	.cfi_startproc*)
(*; %bb.0:|}*)

let global name = ".global " ^ name
let gen_function f = global f.ident
let gen_var _ = ""

let gen_declaration decl =
  match decl with
  | FunctionDeclaration f -> gen_function f
  | GlobalVarDeclaration var -> gen_var var

let program prog buf =
  let rec loop p =
    match p with
    | Program [] -> Buffer.contents buf
    | Program (decl :: rest) ->
        Buffer.add_string buf (gen_declaration decl);
        loop (Program rest)
  in
  loop prog
