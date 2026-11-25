# Code generation

`lib/gen.ml` turns the syntax tree into ARM64 assembly text for macOS. This
document explains the design; the source is commented to be read alongside it.

## ARM64 in five minutes

ARM64 (also called AArch64) has 31 general-purpose 64-bit registers, `x0`-`x30`,
plus a stack pointer `sp`. The same registers can be used as 32-bit with the
names `w0`-`w30`. Instructions are fixed 4 bytes and, like most RISC machines,
**only `ldr`/`str` touch memory**; everything else works on registers.

```
mov  x0, #5            x0 = 5
add  x0, x0, x1        x0 = x0 + x1
ldr  x0, [x1]          x0 = *(long *)x1
str  w0, [x1]          *(int *)x1 = w0
ldr  x0, [sp], #16     x0 = *sp; sp += 16          (post-increment: "pop")
str  x0, [sp, #-16]!   sp -= 16; *sp = x0          (pre-decrement:  "push")
cmp  x0, x1            compare, setting flags
cset x0, lt            x0 = (flags say "less than") ? 1 : 0
b    label             goto label
cbz  x0, label         if (x0 == 0) goto label
bl   _puts             call: x30 = return address; goto _puts
ret                    goto x30
```

Registers with special roles in the Apple ABI:

| Register | Role |
| --- | --- |
| `x0`-`x7` | the first 8 arguments; `x0` also holds the return value |
| `x9`-`x15`, `x16`, `x17` | scratch: a called function may overwrite them |
| `x19`-`x28` | callee-saved: a function must restore them (we never use them) |
| `x29` | frame pointer |
| `x30` | link register: return address, set by `bl` |
| `sp` | stack pointer; **must be 16-byte aligned** whenever it is used for memory access |

## The design: a stack machine

The simplest correct way to compile expressions is: **every expression leaves
its value in `x0`**. To combine two sub-expressions, evaluate the left one,
push `x0` on the stack, evaluate the right one, and pop the left value into
another register.

For `a + b` the generator emits (`gen_binary`):

```
    <code for a>          ; result in x0
    str x0, [sp, #-16]!   ; push it
    <code for b>          ; result in x0
    mov x1, x0            ; x1 = b
    ldr x0, [sp], #16     ; x0 = a  (pop)
    add x0, x0, x1        ; x0 = a + b
```

Because every sub-expression follows the same contract ("value ends up in
`x0`, and `sp` is back where it started"), any expression can be nested inside
any other and it just works. A function call inside the right operand cannot
clobber the saved left operand, because it is on the stack, not in a register.

The price is many redundant loads and pushes. A real compiler would keep values
in registers; this one trades speed for being easy to read and hard to get
wrong. Registers used: `x0` result, `x1` right operand, `x2`/`x9` scratch,
`x16` for large constants.

Each push uses a 16-byte slot although a value only needs 8, because `sp` must
stay 16-byte aligned.

### Everything is a 64-bit value in `x0`

Whatever the C type, a value in `x0` is **sign-extended to 64 bits**. The type
only matters in three places:

* *loading and storing* (`emit_load`/`emit_store`): `char` is `ldrsb`/`strb`
  (load signed byte / store byte), `int` is `ldrsw`/`str w`, `long` and pointers
  are plain `ldr`/`str`;
* *after arithmetic that can overflow*: an `int` result goes through `sxtw`
  (sign-extend the low 32 bits) so that `INT_MAX + 1` wraps to `INT_MIN`;
* *conversions* (`emit_convert`): a cast to `char` is `sxtb`, to `int` `sxtw`;
  casting to `long` needs nothing, since the value is already 64-bit.

### Types follow the code

`gen_expr` returns the C type of the expression it generated, and the type
drives the code. That is how `p + 1` knows to add 4 for an `int *` but 1 for a
`char *` (`emit_binop`), why `p - q` is divided by the element size, and why
`a == b` on pointers uses unsigned comparison (`lo`/`hi` instead of `lt`/`gt`).

The types come from `Ast.type_spec`. Sizes (`sizeof`): `char` 1, `int` 4, `long`
and pointers 8, arrays `n * element`.

**Arrays decay.** In most expressions an array name means "the address of its
first element", so `gen_expr` returns a pointer type for arrays. The exceptions,
`sizeof array` and `&array`, use `gen_addr`, which returns the un-decayed type.
`sizeof` does not evaluate its operand: the generator runs on it with output
redirected into a throwaway buffer (`with_output`) just to learn its type.

### Lvalues: `gen_addr`

`gen_expr` computes a *value*; `gen_addr` computes an *address* (an "lvalue" in
C). It handles variables and `*p`; `a[i]` arrives already rewritten by the
parser as `*(a + i)`. Reading a variable is `gen_addr` followed by a load;
assignment is `gen_addr`, push, evaluate the right side, pop the address, store.

## Stack frames

Each function has a frame, built by its **prologue** and torn down by its
**epilogue**. The stack grows toward lower addresses:

```
        higher addresses
        ┌──────────────────────────┐
        │ caller's frame           │
        ├──────────────────────────┤
        │ saved x30 (return addr)  │
        │ saved x29 (caller's fp)  │ ◄── x29 (our frame pointer)
        ├──────────────────────────┤
        │ local variables          │   at x29 - offset
        │ (parameters first)       │
        ├──────────────────────────┤
        │ temporaries              │ ◄── sp moves up and down in here
        └──────────────────────────┘      (the push/pop of expressions)
        lower addresses
```

Locals are addressed relative to `x29`, which does not move while temporaries
come and go. `declare_local` hands out offsets: each variable gets its own slot
(aligned to its type) for the whole function; slots are not reused after a
block ends.

A worked example. This C:

```c
int add(int a, int b)
{
    int sum = a + b;
    return sum * 2;
}
```

becomes (`malacca -S`), with comments added:

```asm
.globl _add                      ; make the symbol visible to the linker
.p2align 2                       ; align code to 4 bytes
_add:                            ; C names get a leading underscore on macOS
    stp x29, x30, [sp, #-16]!    ; prologue: push fp and return address
    mov x29, sp                  ;           our frame pointer
    sub sp, sp, #16              ;           room for locals (a, b, sum = 12 bytes, rounded to 16)
    sub x9, x29, #4              ; spill parameter a (arrived in w0) to [x29-4]
    str w0, [x9]
    sub x9, x29, #8              ; spill parameter b to [x29-8]
    str w1, [x9]
                                 ; int sum = a + b;
    sub x0, x29, #4              ;   x0 = &a
    ldrsw x0, [x0]               ;   x0 = a        (load 32-bit, sign-extend)
    str x0, [sp, #-16]!          ;   push
    sub x0, x29, #8
    ldrsw x0, [x0]               ;   x0 = b
    mov x1, x0
    ldr x0, [sp], #16            ;   pop a
    add x0, x0, x1
    sxtw x0, w0                  ;   wrap to 32 bits like a C int
    sub x1, x29, #12             ;   x1 = &sum
    str w0, [x1]                 ;   sum = x0
                                 ; return sum * 2;
    ... (same pattern for sum * 2) ...
    b Lret1                      ;   jump to the epilogue
    mov x0, #0                   ; (reached only by falling off the end of the function)
Lret1:
    mov sp, x29                  ; epilogue: discard temporaries and locals
    ldp x29, x30, [sp], #16      ;           restore fp and return address
    ret
```

Notes:

* The function body is generated **before** the prologue is written
  (`gen_function`), because the prologue needs the final frame size.
* Every `return` jumps to one shared epilogue label.
* Local addresses are always computed with `sub xN, x29, #offset` rather than
  `ldr x0, [x29, #-offset]`, because the latter can only encode offsets down
  to -256 and frames can be larger. (Offsets above 4095 first load the number
  into `x16`; see `emit_local_addr`.)

## Calls and Apple's calling convention

The first 8 arguments go in `x0`-`x7`, the result in `x0`. `gen_call`:

1. evaluates each argument left to right and **pushes** it (loading registers
   as it goes would not work: evaluating the next argument might call a
   function, which overwrites `x0`-`x7`);
2. loads `x0`..`xN` from the pushed slots;
3. `bl _name`;
4. pops everything by adding to `sp`.

### Variadic functions: where Apple differs

For a function with `...`, such as `printf`, the standard ARM64 convention
passes the extra arguments in registers like the others. **Apple's does not**:
all variadic arguments go **on the stack**, in 8-byte slots, lowest address
first, with only the *named* parameters in registers. A compiler that gets this
wrong still compiles `printf("hi\n")` (no extra arguments), but prints garbage
for `printf("%d\n", x)`.

So for `printf("%d %d\n", 1, 2)`, after the pushes `gen_call` also builds a
stack area holding `1` and `2` at `[sp]` and `[sp, #8]` and loads only the
format string into `x0`:

```asm
    adrp x0, l_.str.0@PAGE ...   ; format string address
    str x0, [sp, #-16]!          ; push the three arguments (temporary slots)
    mov x0, #1
    str x0, [sp, #-16]!
    mov x0, #2
    str x0, [sp, #-16]!
    sub sp, sp, #16              ; the variadic area: 2 slots of 8 bytes
    ldr x9, [sp, #32]            ; copy argument "1" into [sp]
    str x9, [sp, #0]
    ldr x9, [sp, #16]            ; copy argument "2" into [sp, #8]
    str x9, [sp, #8]
    ldr x0, [sp, #48]            ; the named parameter, format, goes in x0
    bl _printf
    add sp, sp, #64              ; drop it all
```

(`strings.c` in the tests prints ten variadic arguments to exercise this.)

Only function *names* can be called, and the callee must be declared first
(a prototype or an earlier definition) so that the generator knows its
parameter and return types: it will not guess, as old C did.

## Control flow

Labels are generated with `new_label` (`Lelse12`, `Lendwhile14`...; the leading
`L` makes the assembler treat them as local). Templates:

```
if (c) A else B        while (c) A            for (i; c; s) A
    <c>                top:                       <i>
    cbz x0, else           <c>                top:
    <A>                    cbz x0, end            <c> ; cbz x0, end
    b end                  <A>                    <A>
else:                      b top              cont:
    <B>                end:                       <s>
end:                                              b top
                                              end:
```

`break` and `continue` jump to the innermost entry of the `loops` list (for a
`for`, `continue` goes to the step, not the condition). `&&`, `||` and `?:` are
branches too, so the right operand is not evaluated when the left decides the
answer.

## Data

* **String literals** go into a `__TEXT,__cstring` section, each with a label
  `l_.str.N`, de-duplicated. The address is formed with the two-instruction
  idiom `adrp x0, label@PAGE` / `add x0, x0, label@PAGEOFF` (the address of the
  4 KB page, then the offset inside it) because ARM64 instructions cannot hold
  a 64-bit address.
* **Global variables** live in `__DATA,__data` under the label `_name`, and are
  reached the same way. Their initial values must be constants (`const_eval`
  folds expressions like `3 * 9`); uninitialised ones are zero-filled.

## Errors

Anything the generator rejects goes through `fail`, which raises `Gen.Error`
with `file:line: error: in function 'f': message`. `current_loc` is updated at
each statement from the `Located` wrapper the parser inserted.
