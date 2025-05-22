open Ast

(* ARM64 Assembly Code Generator
 *
 * This module generates ARM64 assembly for macOS from our AST.
 * Key features:
 * - Uses x0-x3 for temporary registers.
 * - Maintains 16-byte stack alignment.
 * - Follows ARM64 ABI calling convention.
 * - x0 holds return values.
 * - x29 is frame pointer.
 * - x30 is link register.
 *)

(* Register allocation - Round-robin through x0-x3. *)
let next_reg = ref 0
let get_next_reg () =
  let reg = !next_reg in
  next_reg := (reg + 1) mod 4;
  reg

(* Stack frame management - Maintains 16-byte alignment. *)
let current_stack_size = ref 0
let get_stack_offset () =
  current_stack_size := !current_stack_size + 16;
  !current_stack_size

(* Variable tracking - Maps variables to stack offsets. *)
let var_offsets = Hashtbl.create 10
let add_var name =
  let offset = get_stack_offset () in
  Hashtbl.add var_offsets name offset;
  offset

let get_var_offset name =
  Hashtbl.find var_offsets name

(* Assembly generation helpers - Standard function prologue/epilogue. *)
let emit_global name = ".global _" ^ name ^ "\n"
let emit_label name = "_" ^ name ^ ":\n"
let emit_function_prologue size =
  "    stp x29, x30, [sp, #-16]!\n" ^  (* Save frame pointer and link register. *)
  "    mov x29, sp\n" ^                 (* Set up frame pointer. *)
  if size > 0 then
    "    sub sp, sp, #" ^ string_of_int ((size + 15) land -16) ^ "\n"  (* Allocate stack space, 16-byte aligned. *)
  else ""

let emit_function_epilogue size =
  if size > 0 then
    "    add sp, sp, #" ^ string_of_int ((size + 15) land -16) ^ "\n" ^  (* Deallocate stack space. *)
    "    ldp x29, x30, [sp], #16\n" ^                  (* Restore frame pointer and link register. *)
    "    ret\n"
  else
    "    ldp x29, x30, [sp], #16\n" ^
    "    ret\n"

(* Expression generation - Handles constants, variables, function calls, and binary ops.
 * Returns a tuple of (assembly_code, result_register) where result_register
 * contains the final value of the expression. *)
let rec gen_expr expr =
  match expr with
  | Const (Int n) ->
      let reg = get_next_reg () in
      "    mov x" ^ string_of_int reg ^ ", #" ^ string_of_int n ^ "\n", reg
  | Const (Float _) ->
      failwith "Float constants not implemented yet."
  | Const (String _) ->
      failwith "String constants not implemented yet."
  | Const (Char c) ->
      let reg = get_next_reg () in
      "    mov x" ^ string_of_int reg ^ ", #" ^ string_of_int (Char.code c) ^ "\n", reg
  | Variable name ->
      let reg = get_next_reg () in
      let offset = get_var_offset name in
      "    ldr x" ^ string_of_int reg ^ ", [x29, #-" ^ string_of_int offset ^ "]\n", reg
  | FunctionCall (func_name, args) ->
      (* Generate code for arguments. *)
      let arg_code_regs = List.map gen_expr args in
      let arg_code = String.concat "" (List.map (fun (code, _) -> code) arg_code_regs) in

      (* Move arguments to argument registers (x0-x7). *)
      let move_args = String.concat ""
        (List.mapi (fun i (_, reg) ->
          if i <> reg then
            "    mov x" ^ string_of_int i ^ ", x" ^ string_of_int reg ^ "\n"
          else "")
          arg_code_regs)
      in

      (* Call the function - Result will be in x0. *)
      arg_code ^ move_args ^ "    bl _" ^ func_name ^ "\n", 0
  | BinaryExpr (op, left, right) ->
      let left_code, left_reg = gen_expr left in
      let right_code, right_reg = gen_expr right in
      let result_reg = get_next_reg () in
      let op_code = match op with
        | Add -> "add"
        | Sub -> "sub"
        | Mult -> "mul"
        | Div -> "sdiv"
        | _ -> failwith "Unsupported operator."
      in
      left_code ^ right_code ^
      "    " ^ op_code ^ " x" ^ string_of_int result_reg ^ ", x" ^
      string_of_int left_reg ^ ", x" ^ string_of_int right_reg ^ "\n",
      result_reg

(* Statement generation - Handles variable declarations, expressions, and returns.
 * For returns, ensures the return value is in x0 as per calling convention. *)
let gen_statement stmt =
  match stmt with
  | VarDeclaration { ident; init; _ } ->
      let offset = add_var ident in
      (match init with
       | Some (FunctionCall _ as expr) ->
           (* For function calls, the result is already in x0. *)
           let expr_code, _ = gen_expr expr in
           expr_code ^
           "    str x0, [x29, #-" ^ string_of_int offset ^ "]\n"
       | Some expr ->
           let expr_code, expr_reg = gen_expr expr in
           expr_code ^
           "    str x" ^ string_of_int expr_reg ^ ", [x29, #-" ^ string_of_int offset ^ "]\n"
       | None -> "")
  | Expression expr ->
      let expr_code, _ = gen_expr expr in
      expr_code
  | ReturnStatement expr_opt ->
      (match expr_opt with
       | Some (FunctionCall _ as expr) ->
           (* For function calls, the result is already in x0. *)
           let expr_code, _ = gen_expr expr in
           expr_code ^
           emit_function_epilogue !current_stack_size
       | Some (Variable name) ->
           (* For variables, load directly into x0. *)
           let offset = get_var_offset name in
           "    ldr x0, [x29, #-" ^ string_of_int offset ^ "]\n" ^
           emit_function_epilogue !current_stack_size
       | Some expr ->
           let expr_code, expr_reg = gen_expr expr in
           expr_code ^
           (if expr_reg <> 0 then
             "    mov x0, x" ^ string_of_int expr_reg ^ "\n"  (* Move result to x0 if not already there. *)
           else "") ^
           emit_function_epilogue !current_stack_size
       | None ->
           emit_function_epilogue !current_stack_size)

(* Function generation - Handles function declarations with proper stack setup.
 * Allocates stack space for parameters and local variables. *)
let gen_function f =
  current_stack_size := 0;  (* Reset stack frame for new function. *)
  Hashtbl.clear var_offsets;

  (* Add parameters to variable tracking. *)
  let param_stores = List.mapi (fun i (Param param) ->
    let offset = add_var param.ident in
    "    str x" ^ string_of_int i ^ ", [x29, #-" ^ string_of_int offset ^ "]\n"
  ) f.params in

  let body_code = String.concat "" (List.map gen_statement f.body) in
  let stack_size = !current_stack_size in

  emit_global f.ident ^ "\n" ^
  ".p2align 2\n" ^
  emit_label f.ident ^
  emit_function_prologue stack_size ^
  String.concat "" param_stores ^
  body_code

let gen_var _ = ""  (* Global variables not implemented yet. *)

let gen_declaration decl =
  match decl with
  | FunctionDeclaration f -> gen_function f
  | GlobalVarDeclaration var -> gen_var var

(* Program generation - Processes all declarations in the program. *)
let program prog buf =
  let rec loop p =
    match p with
    | Program [] -> Buffer.contents buf
    | Program (decl :: rest) ->
        Buffer.add_string buf (gen_declaration decl);
        loop (Program rest)
  in
  loop prog
