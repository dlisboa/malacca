# Overview

## Using it

```sh
dune build                                  # build the compiler
dune test                                   # run the test suite
dune exec malacca -- hello.c                # compile hello.c to ./hello
./hello
```

`dune exec malacca -- ...` runs the freshly built compiler; after `dune build`
the binary is also at `_build/default/bin/main.exe`. Options:

| Option | Effect |
| --- | --- |
| `-o file` | name of the output (default: the input without `.c`) |
| `-S` | stop after code generation and write the assembly (`prog.s`) |
| `-E` | stop after the preprocessor and print the result |
| `--dump-ast` | print the syntax tree and stop |
| `-I dir` | also search `dir` for `#include` files |

`-E`, `--dump-ast` and `-S` expose each stage of the compiler, which is the
best way to see what it is doing with a program.

You need the Xcode command line tools: the compiler uses the system assembler
(`as`) and linker (`ld`), and `xcrun` to find the macOS SDK.

## The pipeline

```
 hello.c
    │
    ▼   lib/preproc.ml           #include, #define, #if ...   (text → text)
 preprocessed text
    │
    ▼   lib/lex.mll              characters → tokens          (ocamllex)
 tokens
    │
    ▼   lib/parse.mly            tokens → syntax tree         (ocamlyacc)
 Ast.program
    │
    ▼   lib/gen.ml               tree → ARM64 assembly text
 hello.s   (in a temp file)
    │
    ▼   as                       assembly → object file       (system tool)
 hello.o
    │
    ▼   ld                       object file + libSystem → executable
 hello
```

The first four stages live in `lib/` and never touch the file system or run
other programs: they are just functions from text to text or tree. The last two
are in `bin/main.ml`, which is a thin command-line wrapper. `lib/compile.ml`
glues the library stages together and turns all failures into one `Error`
exception whose message is ready to print.

Everything the compiler produces from C is assembly *text*. malacca does not
write machine code itself; Apple's assembler does that.

## Source tree

| Path | Role |
| --- | --- |
| `bin/main.ml` | Command line, running `as` and `ld` |
| `lib/ast.ml` | The syntax tree types: the vocabulary shared by parser and generator |
| `lib/lex.mll` | Lexer (tokens, comments, literals) |
| `lib/parse.mly` | Grammar |
| `lib/preproc.ml` | Preprocessor |
| `lib/gen.ml` | Code generator |
| `lib/compile.ml` | The pipeline and error reporting |
| `include/*.h` | The standard headers malacca ships (see [preprocessor.md](preprocessor.md)) |
| `tools/embed_headers.ml` | Build-time helper that embeds `include/*.h` in the compiler |
| `test/` | The end-to-end tests ([testing.md](testing.md)) |
| `test/data/` | Older sample inputs, used for manual experiments (not run by `dune test`) |

`lib/macro_parser.ml` is an unfinished experiment that is excluded from the
build (see `lib/dune`).

## One program, all the way through

```c
int add(int a, int b) { return a + b; }
```

**Tokens** (what the lexer sees): `KEY_INT IDENTIFIER("add") LPAREN KEY_INT
IDENTIFIER("a") COMMA KEY_INT IDENTIFIER("b") RPAREN LBRACE KEY_RETURN
IDENTIFIER("a") PLUS IDENTIFIER("b") SEMICOLON RBRACE EOF`

**Tree** (`malacca --dump-ast`, abridged):

```
FunctionDeclaration { ident = "add"; type_spec = TypeInt;
  params = [Param {type_spec = TypeInt; ident = "a"}; Param {... "b"}];
  body = Some [ ReturnStatement (Some (BinaryExpr (Add, Variable "a", Variable "b"))) ] }
```

**Assembly** (`malacca -S`): see [codegen.md](codegen.md) for a line-by-line
walk-through of a similar function.
