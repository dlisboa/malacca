(* Code generator: Ast.program -> ARM64 (Apple silicon) assembly text.

   See docs/codegen.md for the big picture. The short version:

   - It is a "stack machine". Evaluating any expression leaves its value in
     register x0. To combine two values (a + b) we evaluate a, push x0 on the
     stack, evaluate b, then pop a back into another register. It generates
     more instructions than a real compiler, but each construct maps to a small,
     fixed template, which makes the output easy to follow and always correct.

   - Registers used: x0 = result of the last expression, x1 = right operand,
     x2 = scratch, x9 = scratch, x29 = frame pointer, sp = stack pointer.

   - Integer values are kept sign-extended to 64 bits in x0, whatever their C
     type. The C type only matters when storing to / loading from memory and
     after arithmetic that can overflow.

   - Every function keeps its local variables at fixed negative offsets from x29
     (the "frame pointer"), see [declare_local]. *)

open Ast

exception Error of string

(* ------------------------------------------------------------------------ *)
(* Errors                                                                   *)
(* ------------------------------------------------------------------------ *)

let current_function = ref ""

(* [fail "format %s" arg] raises [Error] with a printf-style message.
   [Printf.ksprintf k fmt] formats like printf and then hands the resulting
   string to the continuation [k] instead of printing it. *)
let fail fmt =
  Printf.ksprintf
    (fun msg ->
      let where =
        if !current_function = "" then "" else "in function '" ^ !current_function ^ "': "
      in
      raise (Error (where ^ msg)))
    fmt

(* ------------------------------------------------------------------------ *)
(* Types                                                                    *)
(* ------------------------------------------------------------------------ *)

(* Sizes follow the arm64 macOS ABI: int is 32 bits, long and pointers 64.
   [void] has size 1, as a GNU extension, so that void* arithmetic works. *)
let rec sizeof = function
  | TypeVoid | TypeChar -> 1
  | TypeInt -> 4
  | TypeLong | Pointer _ -> 8
  | Array (t, n) -> n * sizeof t

let rec alignof = function Array (t, _) -> alignof t | t -> sizeof t

let is_integer = function TypeChar | TypeInt | TypeLong -> true | _ -> false

let align_up n alignment = (n + alignment - 1) / alignment * alignment

(* ------------------------------------------------------------------------ *)
(* Emitting assembly                                                        *)
(* ------------------------------------------------------------------------ *)

(* All instructions are appended to the buffer [out] points to. [out] is a
   [ref] (a mutable cell, like a pointer to a Buffer) so that we can temporarily
   redirect output; see [with_output]. *)
let out = ref (Buffer.create 4096)

(* [emit "mov x0, #%d" 5] appends "\tmov x0, #5\n". *)
let emit fmt =
  Printf.ksprintf (fun s -> Buffer.add_string !out ("\t" ^ s ^ "\n")) fmt

let emit_label name = Buffer.add_string !out (name ^ ":\n")

(* Run [f] with output going to [buf] instead, then restore. *)
let with_output buf f =
  let saved = !out in
  out := buf;
  Fun.protect ~finally:(fun () -> out := saved) f

(* Assembler-local labels start with "L" on macOS and never reach the symbol
   table. A global counter makes them unique. *)
let label_counter = ref 0

let new_label kind =
  incr label_counter;
  Printf.sprintf "L%s%d" kind !label_counter

(* Load a constant into [reg]. A single "mov" can only encode 16 bits, so larger
   values are assembled 16 bits at a time: "movz" sets the first chunk (zeroing
   the rest) and each "movk" patches in one more chunk. *)
let emit_mov_imm reg n =
  if n >= -65536 && n < 65536 then emit "mov %s, #%d" reg n
  else
    let n = Int64.of_int n in
    emit "movz %s, #%Ld" reg (Int64.logand n 0xffffL);
    for i = 1 to 3 do
      let chunk = Int64.logand (Int64.shift_right_logical n (16 * i)) 0xffffL in
      if chunk <> 0L then emit "movk %s, #%Ld, lsl #%d" reg chunk (16 * i)
    done

(* The stack must stay 16-byte aligned whenever we touch memory through sp, so
   every temporary takes a 16-byte slot even though a value only needs 8. *)
let push () = emit "str x0, [sp, #-16]!"

let pop reg = emit "ldr %s, [sp], #16" reg

(* sub/add can only take a 12-bit immediate (0..4095). *)
let adjust_sp op n =
  if n <= 4095 then emit "%s sp, sp, #%d" op n
  else begin
    emit_mov_imm "x16" n;
    emit "%s sp, sp, x16" op
  end

(* ------------------------------------------------------------------------ *)
(* Symbols: variables and functions                                         *)
(* ------------------------------------------------------------------------ *)

type var =
  | Local of { offset : int; ty : type_spec }
      (* lives at x29 - offset *)

(* Block scoping: a stack of tables, innermost first. Entering a { } block
   pushes a table, leaving it pops it. *)
let scopes : (string, var) Hashtbl.t list ref = ref []

let enter_scope () = scopes := Hashtbl.create 8 :: !scopes
let leave_scope () = scopes := List.tl !scopes

let lookup_var name =
  match List.find_map (fun tbl -> Hashtbl.find_opt tbl name) !scopes with
  | Some v -> v
  | None -> fail "'%s' undeclared" name

(* Size of the current function's local-variable area, in bytes. *)
let frame_size = ref 0

(* Reserve stack space for a local variable and return its offset below x29.
   Each variable gets its own slot for the whole function, even if its block
   ends early: wasteful but simple. *)
let declare_local name ty =
  let scope = List.hd !scopes in
  if Hashtbl.mem scope name then fail "redeclaration of '%s'" name;
  frame_size := align_up (!frame_size + sizeof ty) (alignof ty);
  Hashtbl.add scope name (Local { offset = !frame_size; ty });
  !frame_size

type func_sig = { ret : type_spec; params : type_spec list; variadic : bool }

let functions : (string, func_sig) Hashtbl.t = Hashtbl.create 16

(* Labels to jump to for "break" and "continue", innermost loop first. *)
let loops : (string * string) list ref = ref []

(* Where the current function's epilogue is, and its return type. *)
let return_label = ref ""
let return_type = ref TypeVoid

(* ------------------------------------------------------------------------ *)
(* Loads, stores and conversions                                            *)
(* ------------------------------------------------------------------------ *)

(* Put the address of the local at [offset] into [reg]. *)
let emit_local_addr reg offset =
  if offset <= 4095 then emit "sub %s, x29, #%d" reg offset
  else begin
    emit_mov_imm "x16" offset;
    emit "sub %s, x29, x16" reg
  end

(* Replace the address in x0 by the value stored there. Narrow types are
   sign-extended on load ("ldrsb" = load register signed byte), so that x0
   always holds a properly sign-extended 64-bit value. *)
let emit_load ty =
  match ty with
  | TypeChar -> emit "ldrsb x0, [x0]"
  | TypeInt -> emit "ldrsw x0, [x0]"
  | TypeLong | Pointer _ -> emit "ldr x0, [x0]"
  | TypeVoid -> fail "cannot use a void value"
  | Array _ -> ()  (* an array's "value" is its address: nothing to load *)

(* Store x0 into the memory whose address is in [addr]. *)
let emit_store ty addr =
  match ty with
  | TypeChar -> emit "strb w0, [%s]" addr
  | TypeInt -> emit "str w0, [%s]" addr
  | TypeLong | Pointer _ -> emit "str x0, [%s]" addr
  | TypeVoid | Array _ -> fail "cannot assign to this type"

(* Make the value in x0 valid for type [dst] by chopping/sign-extending it. *)
let emit_convert ~src ~dst =
  match (src, dst) with
  | _ when src = dst -> ()
  | TypeChar, TypeInt -> ()  (* already sign-extended *)
  | _, TypeChar -> emit "sxtb x0, w0"
  | _, TypeInt -> emit "sxtw x0, w0"
  | _ -> ()

(* ------------------------------------------------------------------------ *)
(* Expressions                                                              *)
(* ------------------------------------------------------------------------ *)

(* The type of an arithmetic result: C's "usual arithmetic conversions", which
   for us means long if either side is long, otherwise int (char widens). *)
let arith_type a b = if a = TypeLong || b = TypeLong then TypeLong else TypeInt

(* Combine x0 (left) and x1 (right) according to [op], leaving the result in
   x0. Returns the result's type. *)
let emit_binop op lty rty =
  if not (is_integer lty && is_integer rty) then
    fail "arithmetic on non-integer operands is not supported yet";
  let ty = arith_type lty rty in
  (match op with
  | Add -> emit "add x0, x0, x1"
  | Sub -> emit "sub x0, x0, x1"
  | Mult -> emit "mul x0, x0, x1"
  | Div -> emit "sdiv x0, x0, x1"
  | Mod ->
      (* ARM has no remainder instruction: a % b = a - (a / b) * b *)
      emit "sdiv x2, x0, x1";
      emit "msub x0, x2, x1, x0"
  | BitAnd -> emit "and x0, x0, x1"
  | BitOr -> emit "orr x0, x0, x1"
  | BitXor -> emit "eor x0, x0, x1"
  | ShiftL -> emit "lsl x0, x0, x1"
  | ShiftR -> emit "asr x0, x0, x1"  (* arithmetic: keeps the sign *)
  | Lt | Gt | Le | Ge | Eq | NotEq | LogAnd | LogOr ->
      fail "internal error: not an arithmetic operator");
  (* An int result must wrap around at 32 bits like in C. *)
  emit_convert ~src:TypeLong ~dst:ty;
  ty

let rec gen_expr expr : type_spec =
  match expr with
  | Const (Int n) ->
      emit_mov_imm "x0" n;
      if n >= -0x80000000 && n <= 0x7fffffff then TypeInt else TypeLong
  | Const (Char c) ->
      emit_mov_imm "x0" (Char.code c);
      TypeInt  (* in C, a character constant has type int *)
  | Const (Float _) -> fail "floating point is not supported"
  | Variable _ ->
      let ty = gen_addr expr in
      emit_load ty;
      ty
  | UnaryExpr (Negate, e) ->
      let ty = gen_expr e in
      emit "neg x0, x0";
      let ty = arith_type ty TypeInt in
      emit_convert ~src:TypeLong ~dst:ty;
      ty
  | UnaryExpr (BitNot, e) ->
      let ty = gen_expr e in
      emit "mvn x0, x0";
      arith_type ty TypeInt
  | UnaryExpr (LogNot, e) ->
      ignore (gen_expr e);
      emit "cmp x0, #0";
      emit "cset x0, eq";
      TypeInt
  | BinaryExpr (LogAnd, lhs, rhs) ->
      (* a && b: if a is false the answer is 0 and b is never evaluated. *)
      let false_l = new_label "false" and end_l = new_label "end" in
      ignore (gen_expr lhs);
      emit "cbz x0, %s" false_l;  (* "compare and branch if zero" *)
      ignore (gen_expr rhs);
      emit "cbz x0, %s" false_l;
      emit "mov x0, #1";
      emit "b %s" end_l;
      emit_label false_l;
      emit "mov x0, #0";
      emit_label end_l;
      TypeInt
  | BinaryExpr (LogOr, lhs, rhs) ->
      (* a || b: if a is true the answer is 1 and b is never evaluated. *)
      let true_l = new_label "true" and end_l = new_label "end" in
      ignore (gen_expr lhs);
      emit "cbnz x0, %s" true_l;
      ignore (gen_expr rhs);
      emit "cbnz x0, %s" true_l;
      emit "mov x0, #0";
      emit "b %s" end_l;
      emit_label true_l;
      emit "mov x0, #1";
      emit_label end_l;
      TypeInt
  | BinaryExpr (op, lhs, rhs) -> gen_binary op lhs rhs
  | Conditional (cond, yes, no) ->
      let else_l = new_label "else" and end_l = new_label "end" in
      ignore (gen_expr cond);
      emit "cbz x0, %s" else_l;
      let yty = gen_expr yes in
      emit "b %s" end_l;
      emit_label else_l;
      let nty = gen_expr no in
      emit_label end_l;
      (* Integers of any width are already sign-extended in x0, so both arms
         agree on the representation; only the C type needs deciding. *)
      if is_integer yty && is_integer nty then arith_type yty nty else yty
  | Assign (lhs, rhs) ->
      (* Compute where to store first, keep that address on the stack while
         the right-hand side is evaluated, then store. The value of the whole
         expression is the value just assigned. *)
      let lty = gen_addr lhs in
      push ();
      let rty = gen_expr rhs in
      emit_convert ~src:rty ~dst:lty;
      pop "x1";
      emit_store lty "x1";
      lty
  | FunctionCall (name, args) -> gen_call name args
  | _ -> fail "this kind of expression is not supported yet"

(* Comparisons and plain arithmetic. [op] is never && or || (added later). *)
and gen_binary op lhs rhs =
  let lty = gen_expr lhs in
  push ();
  let rty = gen_expr rhs in
  emit "mov x1, x0";
  pop "x0";
  (* Now x0 = left operand, x1 = right operand. *)
  match op with
  | Lt | Gt | Le | Ge | Eq | NotEq ->
      if not (is_integer lty && is_integer rty) then
        fail "comparison of non-integers is not supported yet";
      emit "cmp x0, x1";
      let cond =
        match op with
        | Lt -> "lt" | Gt -> "gt" | Le -> "le" | Ge -> "ge"
        | Eq -> "eq" | _ -> "ne"
      in
      (* cset: set x0 to 1 if the condition holds, else 0. *)
      emit "cset x0, %s" cond;
      TypeInt
  | _ -> emit_binop op lty rty

(* Generate code that leaves the ADDRESS of an lvalue in x0 and returns the
   type of the object found there (which may be an array: unlike gen_expr,
   nothing is decayed here). *)
and gen_addr expr : type_spec =
  match expr with
  | Variable name -> (
      match lookup_var name with
      | Local { offset; ty } ->
          emit_local_addr "x0" offset;
          ty)
  | _ -> fail "expression is not assignable (not an lvalue)"

(* A call follows Apple's arm64 calling convention: the first 8 arguments go in
   x0..x7 and the result comes back in x0.

   We evaluate every argument left to right, pushing each on the stack, and only
   then load them into registers. (Loading as we go would not work: evaluating
   the next argument may itself call a function and clobber x0..x7.) *)
and gen_call name args =
  let sg =
    match Hashtbl.find_opt functions name with
    | Some sg -> sg
    | None -> fail "call to undeclared function '%s'" name
  in
  let nargs = List.length args and nfixed = List.length sg.params in
  if nargs <> nfixed then
    fail "function '%s' takes %d argument(s) but %d given" name nfixed nargs;
  if nargs > 8 then fail "calls with more than 8 arguments are not supported";
  List.iter2
    (fun arg pty ->
      let aty = gen_expr arg in
      emit_convert ~src:aty ~dst:pty;
      push ())
    args sg.params;
  (* The i-th argument sits (nargs - 1 - i) slots above the stack pointer. *)
  for i = 0 to nargs - 1 do
    emit "ldr x%d, [sp, #%d]" i (16 * (nargs - 1 - i))
  done;
  emit "bl _%s" name;
  if nargs > 0 then adjust_sp "add" (16 * nargs);
  sg.ret

(* ------------------------------------------------------------------------ *)
(* Statements                                                               *)
(* ------------------------------------------------------------------------ *)

let rec gen_statement stmt =
  match stmt with
  | Empty -> ()
  | Expression e -> ignore (gen_expr e)
  | VarDeclaration { type_spec; ident; init } -> (
      let offset = declare_local ident type_spec in
      match init with
      | None -> ()
      | Some e ->
          let ety = gen_expr e in
          emit_convert ~src:ety ~dst:type_spec;
          emit_local_addr "x1" offset;
          emit_store type_spec "x1")
  | ReturnStatement None -> emit "b %s" !return_label
  | ReturnStatement (Some e) ->
      let ety = gen_expr e in
      emit_convert ~src:ety ~dst:!return_type;
      emit "b %s" !return_label
  | Block stmts ->
      enter_scope ();
      List.iter gen_statement stmts;
      leave_scope ()
  | If (cond, yes, no) -> (
      let else_l = new_label "else" and end_l = new_label "endif" in
      ignore (gen_expr cond);
      emit "cbz x0, %s" else_l;  (* condition false: skip the "then" part *)
      gen_statement yes;
      match no with
      | None -> emit_label else_l
      | Some no ->
          emit "b %s" end_l;
          emit_label else_l;
          gen_statement no;
          emit_label end_l)
  | While (cond, body) ->
      (* top: if (!cond) goto end;  body;  goto top;  end: *)
      let top = new_label "while" and end_l = new_label "endwhile" in
      emit_label top;
      ignore (gen_expr cond);
      emit "cbz x0, %s" end_l;
      gen_loop_body ~break_l:end_l ~continue_l:top body;
      emit "b %s" top;
      emit_label end_l
  | DoWhile (body, cond) ->
      (* top: body;  continue: if (cond) goto top;  end: *)
      let top = new_label "do" and cont = new_label "docond" in
      let end_l = new_label "enddo" in
      emit_label top;
      gen_loop_body ~break_l:end_l ~continue_l:cont body;
      emit_label cont;
      ignore (gen_expr cond);
      emit "cbnz x0, %s" top;
      emit_label end_l
  | For { init; cond; step; body } ->
      (* init;  top: if (!cond) goto end;  body;  continue: step;  goto top *)
      let top = new_label "for" and cont = new_label "forstep" in
      let end_l = new_label "endfor" in
      enter_scope ();  (* a variable declared in init lives only in the loop *)
      List.iter gen_statement init;
      emit_label top;
      Option.iter
        (fun c ->
          ignore (gen_expr c);
          emit "cbz x0, %s" end_l)
        cond;
      gen_loop_body ~break_l:end_l ~continue_l:cont body;
      emit_label cont;
      Option.iter (fun e -> ignore (gen_expr e)) step;
      emit "b %s" top;
      emit_label end_l;
      leave_scope ()
  | Break -> (
      match !loops with
      | (break_l, _) :: _ -> emit "b %s" break_l
      | [] -> fail "'break' outside of a loop")
  | Continue -> (
      match !loops with
      | (_, continue_l) :: _ -> emit "b %s" continue_l
      | [] -> fail "'continue' outside of a loop")

(* Generate a loop's body with break/continue wired to the given labels. *)
and gen_loop_body ~break_l ~continue_l body =
  loops := (break_l, continue_l) :: !loops;
  gen_statement body;
  loops := List.tl !loops

(* ------------------------------------------------------------------------ *)
(* Functions and the whole program                                          *)
(* ------------------------------------------------------------------------ *)

let signature_of (f : func_declaration) =
  {
    ret = f.type_spec;
    params = List.map (fun (Param p) -> p.type_spec) f.params;
    variadic = f.variadic;
  }

(* Remember a function's signature so later code can call it. Declaring the
   same function twice (prototype, then definition) is fine as long as they
   agree. *)
let register_function (f : func_declaration) =
  let sg = signature_of f in
  (match Hashtbl.find_opt functions f.ident with
  | Some old when old <> sg ->
      fail "conflicting declarations of function '%s'" f.ident
  | _ -> ());
  Hashtbl.replace functions f.ident sg

(* The frame layout while a function runs (stack grows downward):

       high addresses
       | caller's frame             |
       +------------------------------+
       | saved x29, x30 (16 bytes)    |  <- x29 points here
       +------------------------------+
       | locals, at x29 - offset      |
       | (parameters first)           |
       +------------------------------+
       | temporaries (push/pop)       |  <- sp moves in here
       low addresses

   We don't know how big the locals area is until the body has been generated,
   so the body is generated into a separate buffer first. *)
let gen_function (f : func_declaration) body =
  current_function := f.ident;
  frame_size := 0;
  return_label := new_label "ret";
  return_type := f.type_spec;
  scopes := [];
  loops := [];
  enter_scope ();  (* parameters and the outermost locals share one scope *)
  if List.length f.params > 8 then
    fail "functions with more than 8 parameters are not supported";
  (* Parameters arrive in x0..x7; copy each to a stack slot so that they are
     ordinary local variables from here on. *)
  let param_stores = Buffer.create 128 in
  let body_code = Buffer.create 1024 in
  with_output param_stores (fun () ->
      List.iteri
        (fun i (Param p) ->
          if p.ident = "" then fail "parameter %d has no name" (i + 1);
          let offset = declare_local p.ident p.type_spec in
          emit_local_addr "x9" offset;
          match p.type_spec with
          | TypeChar -> emit "strb w%d, [x9]" i
          | TypeInt -> emit "str w%d, [x9]" i
          | _ -> emit "str x%d, [x9]" i)
        f.params);
  with_output body_code (fun () ->
      List.iter gen_statement body;
      (* Falling off the end of a function: return 0. That's what C99 requires
         for main, and harmless elsewhere. *)
      emit "mov x0, #0");
  (* Everything is known now: write the function out, prologue first. *)
  let locals = align_up !frame_size 16 in  (* sp must stay 16-aligned *)
  Buffer.add_string !out
    (Printf.sprintf "\n.globl _%s\n.p2align 2\n_%s:\n" f.ident f.ident);
  emit "stp x29, x30, [sp, #-16]!";  (* save caller's frame ptr + return address *)
  emit "mov x29, sp";  (* our frame pointer *)
  if locals > 0 then adjust_sp "sub" locals;  (* room for the locals *)
  Buffer.add_buffer !out param_stores;
  Buffer.add_buffer !out body_code;
  (* Every return jumps here. Restoring sp from x29 also throws away any
     temporaries still on the stack. *)
  emit_label !return_label;
  emit "mov sp, x29";
  emit "ldp x29, x30, [sp], #16";
  emit "ret";
  current_function := ""

(* The entry point: generate assembly for a whole translation unit. *)
let program (Program decls) =
  Hashtbl.reset functions;
  label_counter := 0;
  let text = Buffer.create 4096 in
  out := text;
  List.iter
    (function
      | FunctionDeclaration f -> (
          register_function f;
          match f.body with Some body -> gen_function f body | None -> ())
      | GlobalVarDeclaration _ -> fail "global variables are not supported yet")
    decls;
  ".text\n" ^ Buffer.contents text
