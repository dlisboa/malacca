# Testing

```sh
dune test
```

builds the compiler and runs every program in `test/programs/`. A test is a
single C file; its expectations are in comments at the top:

```c
// expect: Hello, world!        one line of expected stdout (repeat for more lines)
// exit: 3                      expected exit status (default 0)
// error: undeclared            compilation must FAIL, and stderr must contain this
#include <stdio.h>
int main(void) { printf("Hello, world!\n"); return 3; }
```

For each file the runner (`test/test_malacca.ml`) compiles it with the real
compiler binary, runs the executable, and compares stdout and the exit status.
Failures print what differed. Because the programs are compiled to native code
and executed, this tests the whole pipeline at once, including the assembler,
linker and calling convention, which is where compiler bugs tend to hide.

Tests are named after what they check. Roughly:

| File | Area |
| --- | --- |
| `hello.c` | The project's definition of done |
| `arithmetic.c`, `comparisons.c`, `logical.c` | expressions |
| `variables.c`, `globals.c`, `initializers.c` | storage |
| `if_else.c`, `loops.c`, `recursion.c` | control flow |
| `pointers.c`, `arrays.c`, `chars.c`, `sizeof_casts.c`, `incdec_compound.c` | types and memory |
| `calls.c`, `strings.c`, `edge_cases.c` | calls, ABI, encodings |
| `preprocessor.c` (+ `local_header.h`) | macros and includes |
| `realistic.c` | a larger program mixing everything |
| `error_*.c` | diagnostics |

## Adding a test

1. Create `test/programs/your_test.c` with the expectation comments.
2. Run `dune test`. Run it *before* implementing the feature to see it fail.

To investigate a failure, compile the file by hand and look at each stage:

```sh
dune exec malacca -- -E test/programs/x.c         # preprocessor output
dune exec malacca -- --dump-ast test/programs/x.c # syntax tree
dune exec malacca -- -S -o x.s test/programs/x.c  # assembly; read x.s
```

`test/data/` holds older sample inputs from the early lexer/parser work; they
are not run by `dune test` (some, like `errors.c`, are deliberately invalid).
