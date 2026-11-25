# Limitations and next steps

malacca compiles a useful subset of C89 plus some C99 (`//` comments, declarations
anywhere in a block, `for (int i...)`).

## Supported

* Types: `char`, `int`, `long`, `void`, pointers, arrays (including arrays of
  arrays), `const` (accepted, ignored)
* Functions with up to 8 parameters, recursion, prototypes, calling variadic
  functions such as `printf`
* Global and local variables, with initializers; array initializer lists and
  string initializers
* All the C operators except `,` (comma), plus `sizeof` and casts
* `if`, `while`, `do`, `for`, `break`, `continue`, `return`, blocks
* Preprocessor: includes, macros, conditionals

## Not supported yet

| Missing | Notes |
| --- | --- |
| `struct`, `union`, `enum`, `typedef` | Needs symbol table of types; the single biggest gap. It is also what blocks the real system headers. |
| `switch`/`case`, `goto`, labels | `switch` is the natural next control-flow addition |
| `float`, `double` | Lexed, but code generation says "floating point is not supported" |
| `unsigned`, `short`, `signed`, `_Bool` | All integer arithmetic is signed. Unsigned needs different division, shifts, comparisons and loads. |
| `static`, `extern`, `inline`, `register`, `volatile` | Not even parsed |
| Defining functions with more than 8 parameters, or calling with more | Apple's convention packs stack arguments by size |
| Defining variadic functions (`va_list`) | Calling them works |
| Function pointers | Calls must name a function |
| The comma operator, `->`, `.` | Structs first |
| Nested initializer lists, `{0}` for a struct | |
| Preprocessor: `#`, `##`, variadic macros | |
| Types are only loosely checked | Passing a `char *` where an `int` is expected is accepted. Conversions between integers and pointers are silent. |
| Optimisation | Every expression goes through the stack; the output is correct but slow. |

## Known quirks

* `a + b = c` is accepted and means `a + (b = c)`; C rejects it.
* Statements after a `return` are generated but unreachable.
* The code generator keeps global state in refs; it can compile one translation
  unit per process (which is how the command line uses it).
* Error messages from the parser say only "syntax error" with a position.

## Ideas, roughly in order of payoff

1. `typedef`, `enum` and `struct`: the AST needs a type environment, and the
   parser needs to know which identifiers are type names (the classic "lexer
   hack").
2. `switch`.
3. `unsigned` and `short`.
4. A register allocator or at least peephole cleanups (`push; mov; pop`
   sequences for simple operands) if speed ever matters.
5. Floating point (`d0`-`d7` registers; separate calling convention rules).
6. Better parse errors (switch from `ocamlyacc` to `menhir`, already installed,
   which can say what token it expected).
